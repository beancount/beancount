"""
Routines to render a generic ASCII tree of nodes.

Note: this is generic code; it could be shared/moved/reused.
"""
from collections import namedtuple


from beancount2.data import Account


class NodeAdapter:

   def create_node(self, name):
       """Return a new node, given its name."""
       raise NotImplementedError

   def get_name(self, node):
       """Return the name of a node."""
       return node.name

   def get_children(self, node):
       """Return a list of the children of a node."""
       return node.children


class TreeDict(dict):
    """A builder and container for a hierarchy of accounts, that can create
    a tree based on being presented with a list of names with components.
    This is applied to the list of accounts here, but is otherwise generic.
    The keys MUST be strings, with a known separator."""

    def __init__(self, node_adaptor, name_separator):
        """
        Args:
          fcreate: A constructor of nodes. Takes one name as parameter.
          fname: A function to get the name of a node.
          fchildren: A function to get the children of a node.
          name_separator: Separator string for components of a name.
        """

        # Implement specific methods on the nodes.
        self.adaptor = node_adaptor

        # The separator string in the name.
        self.separator = name_separator

        # A mapping of (name, node).
        dict.__setitem__(self, '', self.adaptor.create_node(''))

    def get_root(self):
      return self['']

    def get_leaf_name(self, name):
      return name.split(self.separator)[-1]

    def get_parent_name(self, name):
      return self.separator.join(name.split(self.separator)[:-1])

    def get_create(self, name):
        """Get or create the account with the given name."""
        assert isinstance(name, str)
        try:
            account = dict.__getitem__(self, name)
        except KeyError:
            parent = self.get_create(self.get_parent_name(name))
            account = self.adaptor.create_node(name)
            self.adaptor.get_children(parent).append(account)
            dict.__setitem__(self, name, account)
        return account

    def render_lines(self, start_node_name=None):
        """Yield a tree-rendering prefix and a node, so that you can print
        this out in a tree."""
        if start_node_name is None:
           start_node = self.get_root()
        else:
           start_node = self[start_node_name]
        return render(start_node, self.adaptor)


# A class to hold the recursion rendering context.
Context = namedtuple('Context', 'adaptor pfx_last pfx_nonleaf pfx_cont pfx_skip')


def render(root, node_adaptor):
    """Render a tree of nodes and return a string for the tree.

    Args:
      root: The root node of the hierarchy.
      node_adaptor: An adaptor for nodes, to get the name and children.
    Yields:
      Pairs of (line, node).
    """

    # Create a rendering context.
    context = Context(node_adaptor, '`-- ', '|-- ', '|   ', '    ')

    # Render as lines and join in a single string.
    for tuples in _render_node(root, context, True, ''):
      yield tuples


def _render_node(node, context, is_last, prefix):
    """Internal recursive node rendering function.
    Yields (the first line, the subsequent line, the node)."""

    children = context.adaptor.get_children(node)

    # Make sure we render the graph in a consistent order.
    children.sort()

    # Render the current line.
    node_name = context.adaptor.get_name(node)
    if node_name:
        line_prefix = prefix + (context.pfx_last if is_last else context.pfx_nonleaf)
        child_prefix = prefix + (context.pfx_skip if is_last else context.pfx_cont)

        # Compute the text to render for the first line.
        line = line_prefix

        # Compute continuation lines.
        line_next = (prefix +
                     (context.pfx_skip if is_last else context.pfx_cont) +
                     (context.pfx_skip if not children else context.pfx_cont))

        yield (line, line_next, node_name, node)

    else:
        child_prefix = ''

    # Loop over all children.
    last_i = len(children) - 1
    for i, child in enumerate(children):
        # Render the child's lines (recurse).
        for line_node in _render_node(child, context, i == last_i, child_prefix):
          yield line_node
