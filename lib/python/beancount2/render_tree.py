"""
Routines to render a generic ASCII tree of nodes.

Note: this is generic code; it could be shared/moved/reused.
"""
from collections import namedtuple


# A class to hold the recursion rendering context.
Context = namedtuple('Context', 'getname getchildren pfx_leaf pfx_nonleaf pfx_cont pfx_skip')


def render(root, fgetname=None, fgetchildren=None):
    """Render a tree of nodes and return a string for the tree.

    Args:
      root: The root node of the hierarchy.
      fgetname: A function to get the name of a node.
      fgetchildren: A function to obtain the list of children.
    Returns:
      A rendered string of the ASCII tree.
    """

    # Set reasonable defaults for a typical Python node implementation.
    if fgetname is None:
        fgetname = lambda x: x.name
    if fgetchildren is None:
        fgetchildren = lambda x: x.children

    # Create a rendering context.
    context = Context(fgetname, fgetchildren,
                      '`-- ', '|-- ', '|   ', '    ')

    # Render as lines and join in a single string.
    lines = _render_node(root, context, True, '')
    return '\n'.join(lines)


def _render_node(node, context, is_leaf, prefix):
    """Internal recursive node rendering function."""

    # Render the current line.
    lines = [prefix +
             (context.pfx_leaf if is_leaf else context.pfx_nonleaf) +
             context.getname(node)]

    # Compute a prefix for all of this node's children.
    child_prefix = prefix + (context.pfx_skip if is_leaf else context.pfx_cont)

    # Loop over all children.
    children = context.getchildren(node)
    last_i = len(children) - 1
    for i, child in enumerate(children):
        # Render the child's lines (recurse).
        child_lines = _render_node(child, context, i == last_i, child_prefix)
        lines.extend(child_lines)

    return lines
