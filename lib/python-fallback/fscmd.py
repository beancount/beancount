#!/usr/bin/env python
"""
A command object that provides commands to work interactively on a tree
hierarchy of nodes similar to a filesystem. The underlying backend is abstracted
and only a few functions have to be provided in order to implement it.
"""

# stdlib imports
import sys, os, cmd, optparse, traceback, logging
from operator import itemgetter
from itertools import imap
from os.path import *



class ResilientParser(optparse.OptionParser):
    "An options parser that does not exit when there is an error."

    def exit(self, status=0, msg=None):
        raise optparse.OptParseError(msg)

class HierarchicalCmd(cmd.Cmd, object):
    """
    A version of Cmd which allows the user to navigate some hierarchy siimlar to
    a file hierarchy.
    """

    prompt = 'bean> '

    def __init__(self):
        cmd.Cmd.__init__(self)

        # The home directory (by default it is the root node).
        self.home = self.get_root_node()

        # The current working directory.
        self.cwd = None
        self.cwdpath = None
        self.cd(self.home)

## FIXME: todo add default and completion handling.
    ## def default(self, line):
    ##     trace('DEFAULT', line)

    ## def completedefault(self, text, line, begidx, endidx):
    ##     pass

    def cmdloop(self):
        while 1:
            try:
                cmd.Cmd.cmdloop(self)
            except Exception, e:
                traceback.print_exc()
                self.stdout.write(str(e) + '\n')
            except KeyboardInterrupt:
                print '\n(Interrupted).'
                pass
            else:
                print
                break

    def parseline(self, line):
        cmd, args, line = super(HierarchicalCmd, self).parseline(line)
        args = splitargs(args)

        parser = getattr(self, 'parser_%s' % cmd, None)
        if parser is not None:
            opts, args = parser.parse_args(args)
        else:
            opts = None

        return cmd, (opts, args), line

    def emptyline(self):
        pass

    def perr(self, s):
        sys.stderr.write("Error: %s\n" % s)



    # ----- Commands ----------------------------------------------------------

    def do_exit(self, _):
        "Exit the shell."
        return True
    do_EOF = do_quit = do_exit

    def do_pwd(self, _):
        "Print name of current/working node."
        path = self.getpath(self.cwd)
        self.stdout.write(path + '\n')

    def do_cd(self, (opts, args)):
        "Change the working node."
        if len(args) > 1:
            return self.perr("Too many arguments.")

        args = [expand(arg, self.cwdpath) for arg in args]
        if not args:
            new_node = self.home
            self.do_pwd(None)

        else:
            node = self.getnode(args[0])
            if node is not None:
                new_node = node
            else:
                return self.perr("Invalid path.")
        self.cd(new_node)

    parser_ls = ResilientParser()
    parser_ls.add_option('-l', '--long', action='store_true',
                         help="Use a long listing format.")
    parser_ls.add_option('-R', '--recursive', action='store_true',
                         help="List nodes recursively.")
    parser_ls.add_option('-r', action='store_true',  # ignored
                         help=optparse.SUPPRESS_HELP)
    parser_ls.add_option('-t', action='store_true',  # ignored
                         help=optparse.SUPPRESS_HELP)

## FIXME: implement -d option.

    def do_ls(self, (opts, args)):
        "List contents of the nodes passed in as arguments."
        args = [expand(arg, self.cwdpath) for arg in args]

        simple_walk = lambda x: imap(itemgetter(0), self.walk(x))
        iterfun = simple_walk if opts.recursive else self.listdir

        fmt = self.fmt_long if opts.long else self.fmt_short

        if not args:
            args = (self.cwdpath,)
        for arg in args:
            write = self.stdout.write

            node = self.getnode(arg)
            if node is None:
                self.perr("ls: cannot access %s: No such node." % arg)
                continue

            if not opts.recursive:
                for child in iterfun(node):
                    m = self.stat(child)
                    if m is None:
                        logging.error("Could not stat '%s'." % child)
                    write(fmt % m + '\n')
            else:
                for child in iterfun(node):
                    m = self.stat(child)
                    if m is None:
                        logging.error("Could not stat '%s'." % child)
                    m['name'] = self.getpath(child, relto=node)
                    write(fmt % m + '\n')

    def do_stat(self, (opts, args)):
        "Stat the node and print the output."

        args = [expand(arg, self.cwdpath) for arg in args]
        for arg in args:
            node = self.getnode(arg)
            if node is None:
                self.perr("Node '%s' does not exist." % arg)
            print
            print '%s:' % arg
            for x in sorted(self.stat(node).iteritems()):
                print '%s: %s' % x
        if args:
            print

    def do_mkdir(self, (opts, args)):
        "Make nodes."
        if not args:
            return self.perr("You must provide at least a node name.")
        dn = expand(args[0], self.cwdpath)
        self.mkdir(dn, *args[1:])

    def do_rmdir(self, (opts, args)):
        "Remove nodes."
        if not args:
            return self.perr("You must provide at least a node name.")
        dn = expand(args[0], self.cwdpath)
        self.rmdir(dn, *args[1:])

    parser_rm = ResilientParser()
    parser_rm.add_option('-r', '--recursive', action='store_true',
                         help="Remove nodes recursively.")
    parser_rm.add_option('-f', '--force', action='store_true',
                         help="Remove nodes and their contents.")

    def do_rm(self, (opts, args)):
        "Remove nodes or node contents."
        if not args:
            return self.perr("You must provide the names of the nodes to remove.")
        args = [expand(arg, self.cwdpath) for arg in args]
        for arg in args:
            node = self.getnode(arg)
            if node is None:
                self.perr("Node '%s' not found." % arg)
                continue

            if opts.recursive:
                for wnode, subnodes in self.walk(node, topdown=False):
                    if opts.force:
                        self.remove_node_contents(wnode)
                    self.remove(wnode)
            else:
                if opts.force:
                    self.remove_node_contents(wnode)
                self.remove(node)

    def do_mv(self, (opts, args)):
        "Move (rename) node."
        if len(args) < 2:
            return self.perr("You must provide naems of nodes to move and target node.")

        src_paths = [expand(x, self.cwdpath) for x in args[:-1]]
        dst_path = expand(args[-1], self.cwdpath)

        # Check all source nodes first.
        src_nodes = []
        for src_path in src_paths:
            node = self.getnode(src_path)
            if node is None:
                return self.perr("Source '%s' does not exist." % src_path)
            src_nodes.append(node)

        if len(src_nodes) > 1:
            # Insure that the destination exists.
            dst_node = self.getnode(dst_path)
            if dst_node is None:
                return self.perr("Target '%s' does not exist." % dst_path)

            # We do the actual reparenting after all nodes verify.
            for node in src_nodes:
                self.set_parent(node, dst_node)

        else:
            dst_node = self.getnode(dst_path)
            if dst_node is not None:
                # This is a move into a directory that already exists.
                parent = dst_node
                name = None

            else:
                # This is a rename. The parent of the renamed node must exist.
                dst_node = self.getnode(dirname(dst_path))
                if dst_node is None:
                    return self.perr("Cannot move into '%s', no such file or directory." %
                                     dirname(dst_path))
                parent = dst_node
                name = basename(dst_path)

            if parent:
                node = src_nodes[0]
                old_parent = self.get_node_parent(node)
                if parent != old_parent:
                    self.set_parent(node, parent)

                if name is not None:
                    self.set_name(node, name)

    def do_debug(self, (opts, args)):
        for x in self.walk():
            print x

    def do_tree(self, (opts, args)):
        trace('FIXME: not implemented.')


    #---------------------------------------------------------------------------
    # Various functions tha deal with the conversion of pathnames to nodes and
    # vice-versa.

    def getpath(self, node, relto=None):
        """
        Given a node-id, return the absolute path it corresponds to.
        If 'relto' is specified, return the path relative to the given node.
        """
        assert isinstance(node, int)

        # Walk up the tree to obtain all the names.
        names = []
        while node is not None:
            name, parent = self.stat_short(node)
            names.append(name)
            if relto is not None and node == relto:
                break
            node = parent

        # Note: we cut away the root node on purpose. The root node is expected
        # to be unique as well.
        names.pop()

        path = os.sep.join(reversed(names))
        if parent is None:
            path = '/' + path
        return path

    def getnode(self, path):
        """
        Return the id of node given the path. If the node is not found, return
        None. 'path' must be an absolute path.
        """
        # We need to start from the root and
        assert self.isabs(path), "Path '%s' is not absolute." % path
        node = self.get_root_node()
        if path.startswith(os.sep):
            path = path[1:]
        comps = [c for c in path.split(os.sep) if c]
        for c in comps:
            node = self.get_child_node(node, c)
            if node is None:
                break
        return node

    def cd(self, node):
        self.cwd = node
        self.cwdpath = self.getpath(self.cwd)
        self.prompt = '[bean] %03s %s> ' % (self.cwd, self.cwdpath)

    def isabs(self, path):
        "Return true if the path is absolute (vs. relative)."
        return path.startswith('/')

    def isdir(self, path):
        """
        Return some node object if the given path exists in the database and is
        a container node (directory). Return None if the path is invalid.
        """
        return self.getnode(path) is not None

    def mkdir(self, path, *args):
        """
        Args may optionally contain the security to use for creating the account.
        """
        dn, bn = dirname(path), basename(path)
        parent_node = self.getnode(dn)
        if parent_node is None:
            return self.perr("The parent directory '%s' does not exist." % dn)
        self.create(parent_node, bn, *args)

    def rmdir(self, path, *args):
        """
        Args may optionally contain the security to use for creating the account.
        """
        node = self.getnode(path)
        if node is None:
            return self.perr("The parent directory '%s' does not exist." % dn)
        self.remove(node, *args)

    def walk(self, node=None, topdown=True):
        """
        Generate the tree nodes in-order, starting from the given node (or the
        root node, if not specified). This method yields pairs of (node,
        childlist). If 'topdown' is True, yield the nodes pre-order, otherwise
        yield the nodes post-order.
        """
        if node is None:
            node = self.get_root_node()
        children = self.listdir(node)
        if topdown:
            yield node, children
        for child in children:
            for x in self.walk(child, topdown):
                yield x
        if not topdown:
            yield node, children


    # ----- kernel interface -----------------------------------------------
    # Override this interface to provide the actual storage.

    def stat(self, node):
        "Return all the attributes of the given node, as a dict."
        raise NotImplementedError

    def stat_short(self, node):
        "Return the (name, parent node) of the given node."
        raise NotImplementedError

    def get_root_node(self):
        "Return the root node."
        raise NotImplementedError

    def get_node_parent(self, node):
        "Return the parent of the given node."
        raise NotImplementedError

    def get_child_node(self, parent, name):
        "Given a parent node and a name, return the corresponding child node."
        raise NotImplementedError

    def listdir(self, node):
        "Return a list of the child nodes."
        raise NotImplementedError

    def create(self, parent, name, *args):
        "Create a new node, with the backend specific 'args'."
        raise NotImplementedError

    def remove(self, node):
        "Remove a node. If the node has children, their parent is set to null."
        raise NotImplementedError

    def remove_node_contents(self, node):
        "Remove the contents of a node."
        raise NotImplementedError

    def set_parent(self, node, parent):
        "Change the parent of 'node' to 'parent'."
        raise NotImplementedError

    def set_name(self, node, name):
        "Change the name of 'node' to 'name'."
        raise NotImplementedError


