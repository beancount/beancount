"""Snippets of code to render a report to Latex.
Saved for later.
"""


def print_latex_report(institutions):
    """Print the readable output to stdout.

    Args:
      institutions: A list of Institutions instances.
    """
    print(PREAMBLE.lstrip())

    for institution in institutions:
        print('\\section{%s}' % institution.name)

        if institution.summary:
            print('{%s}\n\n' % institution.summary)
        if institution.address:
            print('Address: {%s}\n\n' % institution.address)

        print('\\begin{itemize}')
        for account_ in institution.accounts:
            print('\\item %s' % account_.name)
        print('\\end{itemize}')

        print()

    print(POSTSCRIPT)


PREAMBLE = r"""
\documentclass[letterpaper,10pt]{article}

\usepackage{fixltx2e}
\usepackage{cmap}
\usepackage{ifthen}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}

\usepackage{times}
\usepackage{fullpage}
\thispagestyle{empty}

\begin{document}
"""

POSTSCRIPT = """
\end{document}
"""
