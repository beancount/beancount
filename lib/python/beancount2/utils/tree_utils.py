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

    def __getitem__(self, name):
        """Get or create the account with the given name."""
        assert isinstance(name, str)
        try:
            account = dict.__getitem__(self, name)
        except KeyError:
            parent = self[self.get_parent_name(name)]
            account = self.adaptor.create_node(name)
            self.adaptor.get_children(parent).append(account)
            dict.__setitem__(self, name, account)
        return account

    def dump(self, out_file, render_node):
        string = render(self.get_root(), self.adaptor, render_node)
        print(string, file=out_file)


# A class to hold the recursion rendering context.
Context = namedtuple('Context', 'adaptor node_custom_data pfx_leaf pfx_nonleaf pfx_cont pfx_skip')


def render(root, node_adaptor, node_custom_data=None):
    """Render a tree of nodes and return a string for the tree.

    Args:
      root: The root node of the hierarchy.
      fname: A function to get the name of a node.
      fchildren: A function to obtain the list of children.
    Returns:
      A rendered string of the ASCII tree.
    """

    # Create a rendering context.
    context = Context(node_adaptor, node_custom_data, '`-- ', '|-- ', '|   ', '    ')

    # Render as lines and join in a single string.
    lines = _render_node(root, context, True, '')
    return '\n'.join(lines)


def _render_node(node, context, is_leaf, prefix):
    """Internal recursive node rendering function."""

    # Render the current line.
    node_name = context.adaptor.get_name(node)
    if node_name:
        line_prefix = prefix + (context.pfx_leaf if is_leaf else context.pfx_nonleaf)
        child_prefix = prefix + (context.pfx_skip if is_leaf else context.pfx_cont)
    else:
        line_prefix = ''
        child_prefix = ''

    # Compute the text to render.
    line = [line_prefix, node_name]
    if context.node_custom_data:
        custom_data = context.node_custom_data(node)
        if custom_data is not None:
            line.append(custom_data)
    lines = [' '.join(line)]

    # Loop over all children.
    children = context.adaptor.get_children(node)
    last_i = len(children) - 1
    for i, child in enumerate(children):
        # Render the child's lines (recurse).
        child_lines = _render_node(child, context, i == last_i, child_prefix)
        lines.extend(child_lines)

    return lines
