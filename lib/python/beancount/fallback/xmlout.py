"""
An XML tree building library for output, much simpler than htmlout, and more
efficient, built on top of ElementTree.

This module adds some niceties from the htmlout module to the tree serialization
capabilities of ElementTree, and is much more efficient than htmlout.
"""

# stdlib imports
import types
from StringIO import StringIO

# elementtree/lxml imports
## from lxml import etree
## from lxml.etree import Element
from elementtree.ElementTree import Element, ElementTree, iselement
from elementtree.ElementTree import _serialize_xml
from elementtree import ElementTree as ElementTreeModule 


class Base(Element):
    "Our base element."

    def __init__(self, *children, **attribs):
        if attribs:
            attribs = translate_attribs(attribs)
        Element.__init__(self, self.__class__.__name__.lower(), attribs)

        self.extend(children)

    def add(self, *children):
        return self.extend(children)

    def extend(self, children):
        "A more flexible version of extend."

        children = flatten_recursive(children)
        if children:
            for child in children:
                # Add child element.
                if isinstance(child, Base):
                    assert iselement(child), child
                    self.append(child)
                    
                # Add string.
                elif isinstance(child, (str, unicode)):
                    if not self._children:
                        if not self.text:
                            self.text = ''
                        self.text += child
                    else:
                        lchild = self._children[-1]
                        if not lchild.tail:
                            lchild.tail = ''
                        lchild.tail += child

                else:
                    raise ValueError("Invalid child type: %s" % type(child))

            return child # Return the last child.


def flatten_recursive(s, f=None):
    """ Flattens a recursive structure of lists and tuples into a simple list."""
    if f is None:
        f = []
    for c in s:
        if isinstance(c, (list, tuple)):
            flatten_recursive(c, f)
        else:
            f.append(c)
    return f


_attribute_trans_tbl = {
    'class_': 'class',
    '_class': 'class',
    'class': 'class',
    'CLASS': 'class',
    'Class': 'class',
    'Klass': 'class',
    'klass': 'class',
    '_id': 'id',
    'id_': 'id',
    'ID': 'id',
    'Id': 'id',
    }

_attribute_translate = _attribute_trans_tbl.get

def translate_attribs(attribs):
    """ Given a dict of attributes, apply a translation table on the attribute
    names. This is made to support specifying classes directly."""
    return dict((_attribute_translate(k, k), v) for k, v in attribs.iteritems())

def tostring(node, *args, **kwds):
    if 'pretty' in kwds or 'pretty_print' in kwds:
        if 'pretty' in kwds: del kwds['pretty']
        if 'pretty_print' in kwds: del kwds['pretty_print']
        indent(node)
    return ElementTree(node).write(*args, **kwds)

# From: http://effbot.org/zone/element-lib.htm#prettyprint
# indent: Adds whitespace to the tree, so that saving it as usual results in a
# prettyprinted tree.
#
# FIXME: This is not going to work if we're sharing nodes (if we have a DAG).
def indent(elem, level=0):
    "in-place prettyprint formatter"
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i



def init(cls):
    allnames = """
     html head body frameset base isindex link meta script style title address
     blockquote center del div h1 h2 h3 h4 h5 h6 hr ins isindex noscript p pre dir
     dl dt dd li menu ol ul table caption colgroup col thead tfoot tbody tr td th
     form button fieldset legend input label select optgroup option textarea a
     applet basefont bdo br font iframe img map area bject param q script span sub
     sup abbr acronym cite code del dfn em ins kbd samp strong var b big i s small
     strike tt u frameset frame noframes noop
    """
    clsdict = {}
    for k in map(str.strip, allnames.split()):
        clsdict[k.upper()] = type(k, (cls,), {})
    return clsdict

clsdict = init(Base)
__all__ = ['tostring'] + clsdict.keys()
globals().update(clsdict)




"""
Implement caching of render results by monkey-patching the serialize functions
from the ElementTree module.
"""

def _serialize_xml_cached(write, elem, encoding, qnames, namespaces):
    if hasattr(elem, 'cache'):
        rendered = elem.cache
        if not isinstance(rendered, (str, unicode)):
            sio = StringIO()
            _serialize_xml(sio.write, elem, encoding, qnames, namespaces)
            rendered = sio.getvalue()
            elem.cache = rendered
        else:
            rendered = '<!-- cached -->' + rendered
        write(rendered)
    else:
        _serialize_xml(write, elem, encoding, qnames, namespaces)

ElementTreeModule._serialize_xml = _serialize_xml_cached
