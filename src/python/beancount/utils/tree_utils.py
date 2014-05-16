"""
Routines to render a generic ASCII tree of nodes.

Note: this is generic code; it could be shared/moved/reused.
"""
from collections import namedtuple


# A class to hold the recursion rendering context.
Context = namedtuple('Context', 'get_name get_children pfx_last pfx_nonleaf pfx_cont pfx_skip')


def render(root, get_name, get_children):
    """Render a tree of nodes and return a string for the tree.

    Args:
      root: The root node of the hierarchy.
      get_name: A function to get the name of the node.
      get_children: A function to get the list of children of the node.
    Yields:
      Tuples of (line, next-line, node-name, node).
    """

    # Create a rendering context.
    context = Context(get_name, get_children, '`-- ', '|-- ', '|   ', '    ')

    # Render as lines and join in a single string.
    for tuples in _render_node(root, context, True, ''):
        yield tuples


def _render_node(node, context, is_last, prefix):
    """Internal recursive node rendering function.
    Yields (the first line, the subsequent line, the node)."""

    children = context.get_children(node)

    # Render the current line.
    node_name = context.get_name(node)
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
