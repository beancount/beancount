"""
Inject some tracing builtins debugging purposes.
"""
__author__ = "Martin Blais <blais@furius.ca>"

# stdlib imports
import sys, os, inspect, pprint, logging
from os import getpid
from os.path import basename


def trace(*args, **kwds):
    """
    Log the object to the 'outfile' file (keyword argument).  We also insert the
    file and line where this tracing statement was inserted.
    """
    # Get the output stream.
    outfile = kwds.pop('outfile', sys.stderr)

    if not kwds.pop('noformat', None):
        msg = _format(args, newline=kwds.pop('newline', False))
    else:
        msg = ' '.join(args)

    # Output.
    outfile.write(msg)
    outfile.flush()


def tracen(*args, **kwds):
    """
    Same as trace(), but output a new line after the trace location.

    Log the object to the 'outfile' file (keyword argument).  We also insert the
    file and line where this tracing statement was inserted.
    """
    # Get the output stream.
    outfile = kwds.pop('outfile', sys.stderr)

    msg = _format(args, newline=True)

    # Output.
    outfile.write(msg)
    outfile.flush()

def tracelog(*args):
    """
    Log the object to the 'outfile' file (keyword argument).  We also insert the
    file and line where this tracing statement was inserted.
    """
    logger = logging.getLogger()
    
    msg = _format(args)

    logger.debug(msg)

def _format(args, newline=0):
    """
    Format the arguments for printing.
    """
    # Get the parent file and line number.
    logging_method = 1
    if not logging_method:
        try:
            stk = inspect.stack()
            frame, filename, lineno, funcname, lines, idx = stk[min(2, len(stk)-1)]
        finally:
            if 'frame' in locals():
                del frame
    else:
        # Use the code from the logging module.
        filename, lineno, funcname = findCaller()

    pfx = '(TRACE [%-5d] %s:%s:%d) ' % (getpid(), basename(filename), funcname, lineno)
    if newline:
        pfx += '\n'

    # Nicely format the stuff to be traced.
    pp = pprint.PrettyPrinter(indent=4, width=70)
    msg = pfx + ', '.join(map(pp.pformat, args)) + '\n'

    return msg


def trace_enter(fun):
    "Decorator for tracing entry and exit of function."

    def wrapped(*args, **kwds):
        targs = [fun.__name__]
        if args:
            targs.append(args)
        if kwds:
            targs.append(kwds)
        trace(*targs)

        return fun(*args, **kwds)

    return wrapped
        

# Inject into builtins for debugging.
import __builtin__
__builtin__.__dict__['trace'] = trace
__builtin__.__dict__['tracen'] = tracen
__builtin__.__dict__['tracelog'] = tracelog
__builtin__.__dict__['trace_enter'] = trace_enter
__builtin__.__dict__['pprint'] = pprint.pprint
__builtin__.__dict__['pformat'] = pprint.pformat


# This is hijacked from logging.py

#
# _srcfile is used when walking the stack to check when we've got the first
# caller stack frame.
#
if hasattr(sys, 'frozen'): #support for py2exe
    _srcfile = "logging%s__init__%s" % (os.sep, __file__[-4:])
elif __file__[-4:].lower() in ['.pyc', '.pyo']:
    _srcfile = __file__[:-4] + '.py'
else:
    _srcfile = __file__
_srcfile = os.path.normcase(_srcfile)

# next bit filched from 1.5.2's inspect.py
def currentframe():
    """Return the frame object for the caller's stack frame."""
    try:
        raise Exception
    except:
        return sys.exc_traceback.tb_frame.f_back

if hasattr(sys, '_getframe'): currentframe = sys._getframe
# done filching

def findCaller():
    """
    Find the stack frame of the caller so that we can note the source
    file name, line number and function name.
    """
    f = currentframe().f_back
    rv = "(unknown file)", 0, "(unknown function)"
    while hasattr(f, "f_code"):
        co = f.f_code
        filename = os.path.normcase(co.co_filename)
        if filename == _srcfile:
            f = f.f_back
            continue
        rv = (filename, f.f_lineno, co.co_name)
        break
    return rv


