"""
Generic utilities.
"""

# stdlib imports
import sys, operator
from time import time
from StringIO import StringIO
from itertools import count, izip, chain, repeat

__all__ = ('render_tree', 'itertree', 'SimpleDummy')


def iter_pairs(l, last=True):
    """Iterate among pairs of items. If last is true, the last item will be
    iterated with the second set to None."""
    i = iter(l)
    b = i.next()
    done = 0
    while not done:
        a = b
        try:
            b = i.next()
        except StopIteration:
            if not last:
                raise
            b = None
            done = 1
        yield a, b

def filter_inout(tlist, pred):
    "Split the list in two according to the given predicate."
    list_in, list_out = [], []
    [(list_in if pred(el) else list_out).append(el) for el in tlist]
    return list_in, list_out


def render_tree(root, pred=None, rootname='.'):
    """
    Generic routine to render a tree of nodes into an cute ascii form. The only
    requirements on each node is that they have a 'name' string attribute and a
    'children' attribute, which should be a list of other nodes. This renders a
    cute tree of dictionaries and reutrn a list of (node, str) pairs to be
    rendered. 'pred' is a predicate that determines which nodes get included.
    """
    lines = [(root, '', rootname)]
    lines.extend(_render_node(root, pred, []))
    return lines

def _render_node(node, pred, pre):
    "Render a dictionary node (recursively)."
    nchildren = len(node.children)
    linesets = []

    last, patcont, patpref = 1, '`-- ', '    '
    for i, sub in enumerate(sorted(node.children, key=lambda x: x.fullname, reverse=1)):
        newlines = _render_node(sub, pred, pre + [patpref])

        if newlines or pred is None or pred(sub):
            if newlines:
                linesets.append(newlines)
            linesets.append( [(sub, ''.join(pre) + patcont, sub.name)] )
            if last:
                last, patcont, patpref = 0, '|-- ', '|   '

    linesets.reverse()
    return reduce(operator.add, linesets, [])


def itertree(root, pred=None):
    """
    Iterate over a tree, producing a labeling of the node and the node itself.
    For example, the following output would be typical:

        ordering   node    isterminal
        ---------- ------- ----------
        (0,)       False   node
        (0,0)      True    node
        (0,1)      True    node
        (1,)       False   node
        (1,0)      False   node
        (1,0,0)    True    node
        (1,0,1)    True    node
        (1,0,2)    True    node

    If the 'pred' predicate is provided, it is used to select nodes from the
    tree.
    """
    # First mark the nodes selected by the predicate. (We use a two-pass
    # algorithm because it would be inefficient in Python to prepend/cons to a
    # list, because it is implement as a vector underneath.)
    if pred is not None:
        markset = set()
        _markpred(root, pred, markset)
    else:
        markset = None
    results = []
    _itertree(root, pred, (0,), results, markset)
    return results

def _markpred(node, pred, markset):
    marked = pred(node)
    for child in node.children:
        marked |= _markpred(child, pred, markset)
    if marked:
        markset.add(node)
    return marked

def _itertree(node, pred, pfx, results, markset):
    "Render a dictionary node (recursively)."
    if (markset is None) or (node in markset):
        results.append( (pfx, not node.children, node) )
        i = 0
        for child in node.children:
            _itertree(child, pred, pfx + (i,), results, markset)
            i += 1



class SimpleDummy(object):
    """
    Simply container object with some conveniences. Just set attrs to declare
    its members. What we want is a version of named_tuple whose members we can
    modify.
    """

    attrs = []

    def __init__(self, *args):
        for a, v in izip(self.attrs, chain(args, repeat(None))):
            setattr(self, a, v)

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__,
                           ', '.join(map(str, self.astuple())))

    def astuple(self):
        return tuple(getattr(self, a) for a in self.attrs)




class TimerUtil:
    "A convenience utility for breaking down time taken in functions."

    class Step:
        def __init__(self, name):
            self.name = name
            self.tfirst = time()
            self.dt = 0

    def __init__(self, timername):
        self.name = timername
        self.tstart = self.tlast = time()
        self.steps = {}
        self.printed = False

    def __call__(self, stepname):
        try:
            step = self.steps[stepname]
        except KeyError:
            step = self.steps[stepname] = TimerUtil.Step(stepname)
        t = time()
        step.dt += t - self.tlast
        self.tlast = t

    def __str__(self):
        s = StringIO()
        steps = sorted(self.steps.values(), key=(lambda s: s.tfirst))
        dtotal = self.tlast - self.tstart
        s.write("\n")
        for step in steps:
            s.write("%s | %s : %.2f  (%.1f%%)\n" % (self.name, step.name, step.dt, step.dt*100/dtotal))
        s.write("%s: %.2f\n" % (self.name, dtotal))
        s.write("\n")
        self.printed = True
        return s.getvalue()

    def __del__(self):
        if not self.printed:
            sys.stdout.write(str(self))
