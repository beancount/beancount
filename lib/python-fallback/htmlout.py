#
#  htmlout -- A simple HTML output library.
#  Copyright (C) 2005  Martin Blais
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""
HTML output builder module.  Valid HTML output is made easy by building an XML
tree of nodes and serializing.  This is a version of the library that uses
ElementTree.

Usage
-----

To build an HTML file you build a tree of XML element using the classes in this
module.  A class with the name of each of the HTML element names is defined, in
capital letters.  By creating instances of these classes and forming a tree you
build the documen tree.  Thus, this module should be import with::

   from htmltree import *

Then all you need do it something like this (an example)::

    doc = HTML(
        META(generator='This Script'),
        LINK(rel='stylesheet', href='style.css', type='text/css')
        )

    body = BODY(parent=doc)

    body.append( P('Some paragraph.') )
    body += P('Some other paragraph.')
    body += ( P('Bla.'),
              P('Bla.') )

    table = TABLE()
    table.append(
        TBODY('ahasa',
              TR( TD('blabla'),
                  TD('blabla'),
                  TD('blabla') )
              ))

    body.append( DIV('text ', A(url, href=url) ' more text') )

    doc += table

Constructor, append and += methods are all equivalent and are processed the same
way, which allows the creation process to be as flexible as possible to
accommodate all use cases:

- other nodes passed in are added as children;

- attributes are set by specifying keyword arguments.  You can also offer a
  dictionary and the key/value pairs will update the attributes list;

- strings are added between child nodes in the order they are seen;

- there is a special attribute 'parent' that can be used to parent the node.
  This is useful when creating a node and storing it in a variable, e.g.::

     body = BODY( ... )
     table = TABLE(parent=body,
                 TR( ...

- tuples are expanded and then processed the same way when used with the +=
  operator. This allows the following syntax::

     table += ( TR( ... ),
                TR( ... ),
                TR( ... ) )

Special attributes
~~~~~~~~~~~~~~~~~~

You can specify the main text content as a keyword attribute by using the
special ``text`` or ``TEXT`` attribute.

You can specify the HTML class by using the ``class_`` or ``CLASS`` keyword
attribute. These are translated into 'class' attributes (the reason for this is
that 'class' is a reserved keyword in Python).


Features
--------

Using such a library presents several advantages:

- tags do not have to be generated in the order that they appear; you can create
  multiple containers, fill them up in parallel and eventually add them to a
  parent container.

- the generated XML is always well-formed;


Inline Styles
-------------

When doing rapid app development, it can be nice to be able to specify chunks of
the stylesheet within the code itself.  Each element has a 'styles' list member
to which you can append stuff.  Then, at output time, if you specify
tostring(styles=1) and you have a STYLE() tag in your document, the sum of all
the style blobs will be put within that tag.


HTML output
-----------

All the nodes are direct descendants of elementtree nodes, and as such, you can
use the serializing operation that elementtree provides to output your XHTML
code.

Validation
----------

The original version of this library had an automatic validation component. This
is not supported right now.


Future TODO
-----------

- we should add a special tag to plop in any kind of html text

"""

# stdlib imports
import xml.dom.minidom
import types

import warnings;
warnings.filterwarnings("ignore", category=FutureWarning, append=1)



def flatten_recursive(s, f=None):
    """
    Flattens a recursive structure of lists and tuples into a simple list.
    """
    if f is None:
        f = []
    for c in s:
        if isinstance(c, types.ListType) or isinstance(c, types.TupleType):
            flatten_recursive(c, f)
        else:
            f.append(c)
    return f


class _NicerElement(xml.dom.minidom.Element):
    """
    An element that renders nicer than the default minidom element.
    This one supports indentation.

    Cut-n-pasted, and then modified, from minidom.py, for compact output
    of elements with a single text child.
    """

    def writexml(self, writer, indent="", addindent="", newl=""):

        # indent = current indentation
        # addindent = indentation to add to higher levels
        # newl = newline string
        writer.write(indent+"<" + self.tagName)

        attrs = self._get_attributes()
        a_names = attrs.keys()
        a_names.sort()

        for a_name in a_names:
            writer.write(" %s=\"" % a_name)
            xml.dom.minidom._write_data(writer, attrs[a_name].value)
            writer.write("\"")
        if self.childNodes:
            if len(self.childNodes) == 1 and\
                   self.childNodes[0].nodeType == \
                   xml.dom.minidom.Node.TEXT_NODE:
                node = self.childNodes[0]

                writer.write(">")
                node.writexml(writer)
                writer.write("</%s>%s" % (self.tagName,newl))
            else:
                writer.write(">%s"%(newl))
                for node in self.childNodes:
                    node.writexml(writer,indent+addindent,addindent,newl)
                writer.write("%s</%s>%s" % (indent,self.tagName,newl))
        else:
            writer.write("/>%s"%(newl))



class _NoopElement(xml.dom.minidom.Element):
    """
    An element that renders only its children and not itself.  All attributes
    set on it will be ignored.
    """

    def writexml(self, writer, indent="", addindent="", newl=""):
        for node in self.childNodes:
            node.writexml(writer,indent,addindent,newl)


class _VerbatimElement: #(xml.dom.minidom.Element):
    """
    An element that renders the given text directly.
    This is not to be used by clients.
    """
    def __init__(self, text):
        self.text = text

    def writexml(self, writer, indent="", addindent="", newl=""):

        # indent = current indentation
        # addindent = indentation to add to higher levels
        # newl = newline string
        writer.write(indent+"<" + self.tagName)

        attrs = self._get_attributes()
        a_names = attrs.keys()
        a_names.sort()

        for a_name in a_names:
            writer.write(" %s=\"" % a_name)
            xml.dom.minidom._write_data(writer, attrs[a_name].value)
            writer.write("\"")
        if self.childNodes:
            if len(self.childNodes) == 1 and\
                   self.childNodes[0].nodeType == \
                   xml.dom.minidom.Node.TEXT_NODE:
                node = self.childNodes[0]

                writer.write(">")
                node.writexml(writer)
                writer.write("</%s>%s" % (self.tagName,newl))
            else:
                writer.write(">%s"%(newl))
                for node in self.childNodes:
                    node.writexml(writer,indent+addindent,addindent,newl)
                writer.write("%s</%s>%s" % (indent,self.tagName,newl))
        else:
            writer.write("/>%s"%(newl))


class Document(xml.dom.minidom.Document):
    """
    Document class.  Just to store some extra options and make pychecker more
    quiet.
    """

    def __init__(self, check_subelems=True, transitional=False):
        xml.dom.minidom.Document.__init__(self)
        self.check_subelems = check_subelems
        self.transitional = transitional

    def abort(self):
        return # "For minidom's DocumentLS spec"

    def load(self, uri):
        return # "For minidom's DocumentLS spec"

    def loadXML(self, source):
        return # "For minidom's DocumentLS spec"



class Base(object):
    """
    Our element types.
    """

    subelems = None

    def __init__(self, *children, **attribs):
        self.cname = self.__class__.__name__.lower()
        self.children, self.text, self.tail, self.attrib = [], '', '', {}
        self.append(*children, **attribs)

        # stylesheet elements stored within the document (can be fetched later)
        # You don't have to use this, but in a rapidapp environment it can be a
        # lot of fun.  See tostring() for more details.
        self.styles = []

    def __iadd__(self, children):
        self.append(*children)
        return self

    def add_class(self, class_):
        """
        Add a class to this element.
        """
        # Note: we should make this a list and append, and at render time do
        # something special rather than concatenate strings here.
        try:
            val = self.attrib['class']
            self.attrib['class'] += ' ' + class_
        except KeyError:
            self.attrib['class'] = class_
        
    def insert(self, idx, *children):
        """
        Insert children.
        FIXME: this does not work with strings yet.
        """
        for child in children:
            # Add child element.
            if isinstance(child, Base):
                self.children.insert(idx, child)
                idx += 1
            else:
                assert child is None
                # Note: we do not yet support inserting text nodes.
                # It wouldn't be hard, there is just no time now.

        return self

    def append(self, *children, **attribs):
        """
        Add children and/or attributes.
        """
        if attribs:
            ##print >> sys.stderr, attribs
            # Treat class attribute specially.
            if 'class_' in attribs:
                attribs['class'] = attribs['class_']
                del attribs['class_']

            if 'CLASS' in attribs:
                attribs['class'] = attribs['CLASS']
                del attribs['CLASS']

            if 'text_' in attribs:
                if not self.text: self.text = ''
                self.text += attribs['text_']
                del attribs['text_']

            if 'TEXT' in attribs:
                if not self.text: self.text = ''
                self.text += attribs['TEXT']
                del attribs['TEXT']

            # Allow adding parent like this "n = NAME(parent=p)"
            if 'parent' in attribs:
                attribs['parent'].append(self)
                del attribs['parent']

            ## # Allow adding an attribute actually named 'parent'
            ## if 'parent_' in attribs:
            ##     attribs['parent'] = attribs['parent_']
            ##     del attribs['parent_']
            ## Note: this is not used, you can use the dict syntax instead.

            for k, v in attribs.items():
                if v is None or v is False:
                    del attribs[k]
                elif v is True:
                    attribs[k] = ''

            # Add all other attributes.
            self.attrib.update(attribs)

        # Expand lists of children, if present, this adds a good convenience in
        # the interface, by allowing the user to intermix instances and lists of
        # them.
        newchildren = flatten_recursive(children)

        #
        # Add children and text.
        #
        for child in newchildren:
            # Add child element.
            if isinstance(child, Base):
                ##print >> sys.stderr, "ELEMENT"
                self.children.append(child)

            # Add string.
            elif isinstance(child, (str, unicode)):
                ##print >> sys.stderr, "STRING"
                if not self.children:
                    if not self.text: self.text = ''
                    self.text += child
                else:
                    lchild = self.children[-1]
                    if not lchild.tail: lchild.tail = ''
                    lchild.tail += child

            # Add attributes in map.
            # This can be useful when there are children put
            # the attributes before the children.
            elif isinstance(child, dict):
                ##print >> sys.stderr, "DICT"
                self.attrib.update(child)

            else:
                assert child is None

        return newchildren

    def extend(self, children):
        return self.append(*children)

    def do_create(self, document):
        """
        Called to create the tree into an XML tree for output.
        """
        if self.cname == 'noop':
            __element = _NoopElement(self.cname)
        else:
            __element = _NicerElement(self.cname)
            
        do_space = False
        if self.text:
            __element.appendChild( document.createTextNode(self.text) )
            do_space = True
            
        # Warning: ugly kludge to make sure that DIVs and TEXTAREAs are never
        # empty.  Empty DIV tags do funny things to browsers.
        elif (not self.children and
              self.cname in ['div', 'ul', 'textarea',
                             'table', 'script', 'iframe']):
            __element.appendChild( document.createTextNode('') )

        for child in self.children:
            if do_space:
                __element.appendChild( document.createTextNode(' ') )
                do_space = False
            xchild = child.do_create(document)
            __element.appendChild(xchild)
            if child.tail:
                if not child.tail.startswith(' '):
                    child.tail = ' ' + child.tail
                __element.appendChild( document.createTextNode(child.tail) )
                do_space = True

        for k, v in self.attrib.items():
            # if attribute has no value, set to null string
            if v is None:
                v = ''
            __element.setAttribute(k, v)

        return __element

    def visit(self, visitor, parent=None):
        """
        Generic visitor pattern.
        """
        if visitor(self, parent):
            # take a copy from the list so we can modify it in the visitor
            for child in list(self.children):
                if child.visit(visitor, self) is False:
                    return False
            return True
        else:
            return False

    def findattr(self, **kwds):
        """
        Find an element by id under the given node.
        """
        assert len(kwds) == 1
        aname, avalue = kwds.items()[0]

        found = []
        def visitor(node, parent):
            if aname in node.attrib and node.attrib[aname] == avalue:
                found.append(node)
                return False
            return True

        self.visit(visitor)
        return found and found[0] or None

class VERBATIM(Base, xml.dom.minidom.Element):
    """
    Element used to enable plopping some text verbatim within a tree of nodes.
    """
    def __init__(self, vtext):
        Base.__init__(self)
        xml.dom.minidom.Element.__init__(self, '')
        self.vtext = vtext

    # This is a method for xml.dom.minidom.Element printing.
    def writexml(self, writer, indent="", addindent="", newl=""):
        writer.write(self.vtext)

    def do_create(self, document):
        # Just return this node to be inserted in the minidom tree.
        return self 



# Also available to scripts.
content_type = "Content-type: text/html\n"

def tostring(root, indent='   ',
             check_subelems=True,
             ctnttype=False, doctype=False, xmldecl=None, encoding=None,
             styles=False, pretty=True):
    """
    Render tag tree into a string of text.
    """

    top = ''
    if ctnttype:
        top += content_type + '\n\n'

    if xmldecl:
        if encoding:
            top += \
                '<?xml version="1.0" encoding="%s" ?>\n' % encoding
        else:
            top += \
                '<?xml version="1.0" ?>\n'

    if doctype:
        top += '<!DOCTYPE html '
        if doctype == 'xhtml-strict':
            top += ('PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"'
                    ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">')
        elif doctype == 'xhtml-transitional':
            top += ('PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' 
                    ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
                    '\n')
        else:
            top += ('PUBLIC "-//W3C//DTD HTML 4.01//EN" '
                    '"http://www.w3.org/TR/html4/strict.dtd">\n')

    if styles:
        # get the style tag
        styletag = get_style_tag(root)
        if styletag:
            # fetch all the little style blobs and concatenate
            allstyles = get_styles(root)

            # put that into to the style tag text
            #
            # Note: we cannot put it between comments here because of the text
            # escaping that occurs in minidom.
            styletag.text = '\n\n' + '\n\n'.join(allstyles) + '\n\n'

    doc = Document(check_subelems, True)
    xroot = root.do_create(doc)
    if pretty:
        return top + xroot.toprettyxml(indent=indent, encoding=encoding)
    else:
        return top + xroot.toxml(encoding=encoding)


def get_styles(node):
    """
    Fetch all the style tags of all the nodes in the tree, and return a list of
    them.
    """

    class getstyle:
        def __init__(self):
            self.styles = []
        def __call__(self, node, parent=None):
            if node.styles:
                assert type(self.styles) is types.ListType
                self.styles.extend(node.styles)
            return True
    gs = getstyle()
    node.visit(gs)
    return gs.styles


def get_style_tag(node):
    """
    Look for the first <style> tag in the tree and return it.
    """

    class getstyle:
        def __init__(self):
            self.found = None
        def __call__(self, node, parent=None):
            if node.cname == 'style':
                self.found = node
                return False
            return True
    gs = getstyle()
    node.visit(gs)
    return gs.found



def get_inputs(node):
    """
    Generator that visits all the INPUTs in the tree.
    """
    l = []
    if isinstance(node, INPUT):
        l.append(node)
    for child in node.children:
        l.extend(get_inputs(child))
    return l


def ischecked(value, ref=None):
    """
    Return the string 'checked' if VALUE is true.
    """
    if ref is None:
        if value:
            return 'checked'
    elif isinstance(ref, str):
        if str(value) == str(ref):
            return 'checked'
    elif isinstance(ref, list):
        for re in ref:
            if str(value) == str(re):
                return 'checked'
    return ''


def replace_input_values(rtree, values):
    """
    Replace the values of INPUT tags in the tree by the values given in the
    'values' mapping, if they are present in it.
    """

    # set the values
    for inp in get_inputs(rtree):
        try:
            name = inp.attrib['name']
            if name not in values:
                continue

            value = str(values[name])
            typ = inp.attrib['type']

            if typ in ('checkbox', 'radio'):
                if ischecked(inp.attrib['value'], value):
                    inp.attrib['checked'] = '1'
                else:
                    del inp.attrib['checked']
            elif typ in ('text', 'hidden', 'password'):
                inp.attrib['value'] = value # encoding?

            else:
                raise RuntimeError("Internal Error, invalid type.")
        except KeyError:
            pass


# Validation code (not needed for now).

## class Error(StandardError):
##     pass

##         if self.doco.check_subelems:
##             subelems = self.subelems_strict
##             if self.doco.transitional:
##                 subelems += self.subelems_transit
##             if not subelems:
##                 raise Error(
##                     "'%s' element cannot contain any other element.'" % \
##                     self.cname)
##             elif child.cname not in subelems:
##                 raise Error(
##                     "'%s' element cannot contain '%s' element.'" %
##                     (self.cname, child.cname))


# main attribute groups
attr_core = ['id', 'class', 'style', 'title']
attr_i18n = ['lang', 'dir']
attr_common = attr_core + attr_i18n

elems_both = ['applet', 'button', 'del', 'iframe', 'ins', 'map',
              'object', 'script']

elems_inline = ['a', 'abbr', 'acronym', 'b', 'basefont', 'bdo', 'big', 'br',
                'cite', 'code', 'dfn', 'em', 'font', 'i', 'img', 'input', 'kbd',
                'label', 'q', 's', 'samp', 'select', 'small', 'span', 'strike',
                'strong', 'sub', 'sup', 'textarea', 'tt', 'u', 'var']

elems_block = ['address', 'blockquote', 'center', 'dir', 'div', 'dl',
               'fieldset', 'form', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr',
               'isindex', 'menu', 'noframes', 'noscript', 'ol', 'p', 'pre',
               'table', 'ul', 'dd', 'dt', 'frameset', 'li', 'tbody', 'td',
               'tfoot', 'th', 'thead', 'tr']

# Mapping of child elements for each element, for both strict and transitional
# doctypes, so that while we're generating the HTML document we're validating
# which child elements are allowed.
#
# Note: this is not exact, but a useful approximation.
# ----------------------------------------------------

elems_map = {
 'html': ['head', 'body', 'frameset'],
 'head': ['title', 'base', 'isindex', 'script', 'style', 'meta', 'link',
          'object'],
 'body': (elems_block + ['script', 'ins', 'del'], elems_inline),
 'frameset': ['frame', 'noframes'],
 'base': [],
 'isindex': [],
 'link': [],
 'meta': [],
 'script': [],
 'style': [],
 'title': [],
 'address': (elems_inline, ['p']),
 'blockquote': (elems_block + ['script'], ['elems_inline']),
 'center': elems_inline + elems_block,
 'del': elems_inline + elems_block,
 'div': elems_inline + elems_block,
 'h1': elems_inline,
 'h2': elems_inline,
 'h3': elems_inline,
 'h4': elems_inline,
 'h5': elems_inline,
 'h6': elems_inline,
 'hr': [],
 'ins': elems_inline + elems_block,
 'isindex': [],
 'noscript': elems_inline + elems_block,
 'p': elems_inline,
 'pre': elems_inline + ['img', 'object', 'applet', 'big', 'small', 'sub', 'sup',
         'font', 'basefont'],
 'dir': ['li'],
 'dl': ['dt', 'dd'],
 'dt': elems_inline,
 'dd': elems_inline + elems_block,
 'li': elems_inline + elems_block,
 'menu': ['li'],
 'ol': ['li'],
 'ul': ['li'],
 'table': ['caption', 'col', 'colgroup', 'thead', 'tfoot', 'tbody'],
 'caption': elems_inline,
 'colgroup': ['col'],
 'col': [],
 'thead': ['tr'],
 'tfoot': ['tr'],
 'tbody': ['tr'],
 'tr': ['th', 'td'],
 'td': elems_inline + elems_block,
 'th': elems_inline + elems_block,
 'form': (['script'] + elems_block, elems_inline),
 'button': elems_inline + elems_block,
 'fieldset': ['legend'] + elems_inline + elems_block,
 'legend': elems_inline,
'input': [],
 'label': elems_inline,
 'select': ['optgroup', 'option'],
 'optgroup': ['option'],
 'option': [],
 'textarea': [],
 'a': elems_inline,
 'applet': ['param'] + elems_inline + elems_block,
 'basefont': [],
 'bdo': elems_inline,
 'br': [],
 'font': elems_inline,
 'iframe': elems_inline + elems_block,
 'img': [],
 'map': elems_block + ['area'],
 'area': [],
 'bject': ['param'] + elems_inline + elems_block,
 'param': [],
 'q': elems_inline,
 'script': [],
 'span': elems_inline,
 'sub': elems_inline,
 'sup': elems_inline,
 'abbr': elems_inline,
 'acronym': elems_inline,
 'cite': elems_inline,
 'code': elems_inline,
 'del': elems_inline + elems_block,
 'dfn': elems_inline,
 'em': elems_inline,
 'ins': elems_inline + elems_block,
 'kbd': elems_inline,
 'samp': elems_inline,
 'strong': elems_inline,
 'var': elems_inline,
 'b': elems_inline,
 'big': elems_inline,
 'i': elems_inline,
 's': elems_inline,
 'small': elems_inline,
 'strike': elems_inline,
 'tt': elems_inline,
 'u': elems_inline,
 'frameset': ['frameset', 'frame', 'noframes'],
 'frame': [],
 'noframes': ('', elems_inline + elems_block),
 'noop': [],
}

__all__ = ['tostring', 'ReRootVisitor', 'VERBATIM']

def init():
    for k, v in elems_map.iteritems():
        n = k.upper()
        newclass = type(n, (Base,), {})

        if isinstance(v, types.TupleType):
            assert len(v) == 2
            newclass.subelems_strict, newclass.subelems_transit = v
        else:
            newclass.subelems_strict, newclass.subelems_transit = v, []

        globals()[n] = newclass
        __all__.append(n)

init()


class ReRootVisitor:
    """
    A visitor for an htmlout tree that will prepend a URL path to all A or IMG
    tags that are local to the server (i.e. they start with /).

    There is a special case: for links that should truly be relative to the
    server root, if they start with two '/', we remove the extra '/' and do not
    perform the rerooting.
    """
    _attribmap = {A: 'href',
                  LINK: 'href',
                  IMG: 'src'}

    def __init__(self, root):
        assert root.startswith('/')
        assert not root.endswith('/')
        self.root = root

    def __call__(self, el, parent):
        typel = type(el)
        if typel in (A, IMG, LINK):
            aname = self._attribmap[typel]
            try:
                url = el.attrib[aname]
                if url.startswith('/'):
                    if url.startswith('//'):
                        el.attrib[aname] = url[1:]
                    else:
                        el.attrib[aname] = self.root + url
            except KeyError:
                pass
        return True



def test():
    # just to make pychecker shut up
    import sys

    doc = HTML(
        HEAD('ankjwajhsjasa',
             META('blabal'),
             STYLE()
        ) )

    body = BODY(
               TABLE(
                   TBODY('ahasa',
                       TR(
                           TD('blabla'),
                           TD('blabla'),
                           TD('blabla'),
                           )
                         ),
                   blabla='somevlue')
               )
    
    body.append( NOOP(P("YEAH"), DIV("prout")), NOOP("proutprout"), NOOP() )

    p1, p2 = P('blabla'), P('bli')

    p2.text += 'dhsdhshkds'
    p2.styles.append('p { font-style: italic; }')


    div = DIV("some random text", id='bli', parent=body)
    div.styles.append("#bli { font-size: xx-large; }")

    doc.append( body )
    doc += (p1, p2)


    url = 'http://furius.ca'
    doc.append(
        DIV( {'name': 'value'},
             P("""Some child tesxtksjddf jkdsdshdshdks dhsd hs
             huhdsudhwiudhsk hdjshdjs dhjksldhssd""",
               A(url, href=url), """more text.""")
             )
        )

    doc.append(
        DIV( VERBATIM("""Some verbatim
        text in multipl>
        lines with >>>>> embedded in them.
        """)))

    sys.stdout.write(tostring(doc, doctype=1, ctnttype=1, styles=1))

if __name__ == "__main__":
    test()
