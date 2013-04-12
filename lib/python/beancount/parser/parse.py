# Copyright (C) 2009-2012  Martin Blais, Inc. All Rights Reserved.
"""
Protocol parser.
"""
import sys
import logging
from os.path import dirname
from collections import namedtuple, defaultdict
from operator import attrgetter
from itertools import chain

import ply.yacc as yacc
import ply.lex as lex

__all__ = ('create_parser',)





tokens = """
    COMMENT INDENT
    DATE NUMBER STRING
    ACCOUNT DRCR CURRENCY
    DEFACCOUNT VAR PAD CHECK BEGINTAG ENDTAG
    TXNFLAG TXN OFX ACCID
    AT ATAT PIPE EQUAL EOL LCURL RCURL COMMA
""".split()


# Completely ignored characters
#t_ignore = ' \t\r'

# Whitespace counts as an 'INDENT' token, only at the beginning of a line.
def t_INDENT(t):
    r'[ \t\r]+'
    if find_column(t) == 0:
        return t


# # Newlines
# def t_NEWLINE(t):
#     r'\n+'
#     t.lexer.lineno += t.value.count("\n")

t_TXN = r'txn'
t_OFX = r'ofx'
t_ACCID = r'accid'

t_PIPE = r'\|'
t_ATAT = r'@@'
t_AT = r'@'
t_LCURL = r'{'
t_RCURL = r'}'
t_EQUAL = r'='
t_COMMA = r','
t_TXNFLAG = r'[*\!&R#c\?SP]'

def t_EOL(t):
    r'\n'
    t.lexer.lineno += 1
    return t

t_DEFACCOUNT = r'@defaccount'
t_VAR = r'@var'
t_PAD = r'@pad'
t_CHECK = r'@check'
t_BEGINTAG = r'@begintag'
t_ENDTAG = r'@endtag'

# def t_DIRECTIVE(t):
#     r'@([a-z]+)'
#     return t

def t_DATE(t):
    r'(\d\d\d\d)[-/](\d\d)[-/](\d\d)'
    return t

# Note: define as function in order to establish an ordering; this is needed here.
def t_ACCOUNT(t):
    r'(([A-Z][A-Za-z0-9\-]+):)+([A-Z][A-Za-z0-9\-]+)'
    return t

def t_CURRENCY(t):
    r"[A-Z][A-Z0-9'\.]{1,10}"
    return t

def t_DRCR(t):
    r'(Dr|Cr)'
    return t

def t_STRING(t):
    r'(".*?")'
    return t

def t_NUMBER(t):
    r'[-+]?([0-9\.])+'
    return t

# def t_ID(t):
#     r'[A-Za-z0-9\-]{8,}'
#     return t




# # Delimiters
# t_LBRACE           = r'\{'
# t_RBRACE           = r'\}'
# t_LT               = r'<'
# t_GT               = r'>'
# t_SEMICOLON             = r';'
# t_COMMA            = r','


# Compute column.
#     input is the input text string
#     token is a token instance
def find_column(t):
    last_cr = t.lexer.lexdata.rfind('\n', 0, t.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (t.lexpos - last_cr) - 1
    return column


# # Preprocessor directive (ignored)
# def t_preprocessor(t):
#     r'\#(.)*?\n'
#     t.lexer.lineno += 1

def t_COMMENT(t):
    r';+(.*)'
    #t.lexer.lineno += t.value.count('\n')
    #return t

def t_error(t):
    print("Illegal character %s" % repr(t.value[0]))
    t.lexer.skip(1)




#===============================================================================
# Grammar.

#-------------------------------------------------------------------------------
# Type declarations.

def p_empty(t):
    'empty : '

def p_txn(p):
    """txn : TXN
           | TXNFLAG
    """
    #varlen(p)

def p_dates(p):
    """dates : DATE
             | DATE EQUAL DATE
    """
    p[0] = p[1] # Ignore the other date for now.


def p_transaction(p):
    """transaction : dates txn STRING EOL posting_list
                   | dates txn STRING PIPE STRING EOL posting_list
    """
    #varlen(p)



Posting = namedtuple('Posting', 'account lot')

def p_optflag(p):
    """optflag : empty
               | TXNFLAG
    """
    p[0] = p[1] if len(p) > 1 else None


def p_posting(p):
    """posting : INDENT optflag ACCOUNT lot EOL
               | INDENT optflag ACCOUNT lot AT amount EOL
               | INDENT optflag ACCOUNT lot ATAT amount EOL
               | INDENT optflag ACCOUNT EOL
    """
    p[0] = Posting(p[2], p[3])



def p_posting_list(p):
    """posting_list : empty
                    | posting_list posting
    """
    handle_varlen(p)




def p_currency_list(p):
    """currency_list : empty
                     | CURRENCY
                     | currency_list COMMA CURRENCY
    """
    handle_varlen(p)




def p_begintag(p):
    """begintag : BEGINTAG STRING
    """

def p_endtag(p):
    """endtag : ENDTAG STRING
    """





VarAccid = namedtuple('VarAccid', 'account accountid')

def p_var_accid(p):
    """var_accid : VAR OFX ACCID STRING ACCOUNT
    """
    p[0] = VarAccid(*p[4:])




DefAccount = namedtuple('DefAccount', 'account drcr currency_list')

def p_defaccount(p):
    """defaccount : DEFACCOUNT DRCR ACCOUNT currency_list
    """
    p[0] = DefAccount(*p[2:])



Pad = namedtuple('Pad', 'date account_to account_from')

def p_pad(p):
    """pad : PAD DATE ACCOUNT ACCOUNT
    """
    p[0] = Pad(*p[2:])




Check = namedtuple('Check', 'date account amount')

def p_check(p):
    """check : CHECK DATE ACCOUNT amount
    """
    p[0] = Check(*p[2:])




Amount = namedtuple('Amount', 'number currency')

def p_amount(p):
    """amount : NUMBER CURRENCY
    """
    p[0] = Amount(p[1], p[2])



Lot = namedtuple('Lot', 'amount cost')

def p_lot(p):
    """lot : amount
           | amount LCURL amount RCURL
    """
    cost = p[3] if len(p) > 2 else None
    p[0] = Lot(p[1], cost)



def p_empty_line(t):
    """empty_line : EOL
                  | INDENT EOL
    """


def p_entry(p):
    """entry : empty_line
             | transaction
             | defaccount
             | var_accid
             | pad
             | check
             | begintag
             | endtag
    """
    p[0] = p[1]

def p_entry_list(p):
    """entry_list : empty
                  | entry_list entry
    """
    handle_varlen(p)




def p_error(p):
    if p is None:
        return logging.error("%s:%d: Error in input." % ('', 0))
    logging.error("%s:%d: Syntax error" % (p.lexer.filename, p.lineno))




start = 'entry_list'


def handle_varlen(p):
    "Do what needs to be done to handle a variable length sequence."
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]] if p[1] else []


#-------------------------------------------------------------------------------

def parse(parser, *args, **kw):
    "Override of PLY's parsing method to allow for side-effects on parsing."
    # parser.refs = []     # All refs created.
    # parser.gentypes = [] # Generated types.
    result = parser.__parse(*args, **kw)
    # if result is None:
    #     return
    # typemap = resolver(result, parser)
    # typemap = runchecks(typemap)
    # return typemap
    return result

def create_parser():
    debug = 0 # Turn this on to regenerate the parser on every run.

    lexer = lex.lex(debug=debug) ## optimize=1
    parser = yacc.yacc(method='LALR', debug=debug, outputdir=dirname(__file__))
    parser.__lexer = lexer

    # Monkey-patch to insure we reset accumulators on startup.
    ParserClass = parser.__class__
    ParserClass.__parse, ParserClass.parse = ParserClass.parse, parse

    return lexer, parser

def runchecks(typemap):
    "Run various checks on the typemap."
    # Check that there aren't any duplicate ids.
    m = groupby((x for x in typemap.itervalues() if isinstance(x, Object)), attrgetter('id'))
    seenerrors = 0
    for id, messages in m.iteritems():
        if len(messages) > 1:
            seenerrors = 1
            logging.error("Duplicate messages: %s" % ', '.join(m.name for m in messages))
    return None if seenerrors else typemap

def resolver(result, parser):
    """ Given an input list of types, resolve all the typerefs.
    Returns None if there were errors."""
    typemap = dict((x.name, x) for x in chain(podtypes, strtypes, result, parser.gentypes))
    haserrors = False
    for r in parser.refs:
        try:
            r.x = typemap[r.name]
        except KeyError:
            logging.error("%s:%d: Type '%s' could not be resolved." % (parser.__lexer.filename, 0, r.name))
            haserrors = True
    return typemap if not haserrors else None
