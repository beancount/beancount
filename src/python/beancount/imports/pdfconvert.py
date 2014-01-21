"""PDF conversion routines.

Many of these fail or are flaky, and they are not consistent, i.e., a converter
tool that fails on a file from one bank may succeed on another, and vice-versa.
Thus we provide many alternative converters that we try in a sequence until
valid output is returned.
"""
import re
import subprocess


def pdfconverter_poppler_pdftotext(pdf_filename):
    """Try to convert the given pdf file it text using pdftotext.

    Args:
      pdf_filename: the name of the PDF file to convert.
    Returns:
      A string of the converted text, if successful, or None, if not.
    """
    p = subprocess.Popen(('pdftotext', pdf_filename, '-'),
                         shell=False,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    if p.returncode == 0 and not stderr:
        # Sometimes the output is just filled with ^L; check that this isn't the
        # case and that the output file has some real content.
        text = stdout.decode()
        if re.search('[a-zA-Z0-9]+', text):
            return text


def pdfconverter_unoconv_plus_pdftotext(pdf_filename):
    """Try to convert the given pdf file it text using unoconv
    and pdftotext.

    Args:
      pdf_filename: the name of the PDF file to convert.
    Returns:
      A string of the converted text, if successful, or None, if not.
    """
    # Try first text conversion using the LibreOffice tool.
    p1 = subprocess.Popen(('unoconv', '--format', 'pdf', '--stdout', pdf_filename),
                          shell=False,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p2 = subprocess.Popen(('pdftotext', '-', '-'),
                          shell=False,
                          stdin=p1.stdout,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout2, stderr2 = p2.communicate()
    if not (p2.returncode != 0 or stderr2):
        return stdout2.decode()


def pdfconverter_ghostscript(pdf_filename):
    """Try to convert the given pdf file it text using ghostscript
    tools pdf2ps + ps2ascii.

    Args:
      pdf_filename: the name of the PDF file to convert.
    Returns:
      A string of the converted text, if successful, or None, if not.
    """
    p1 = subprocess.Popen(('pdf2ps', pdf_filename, '-'),
                          shell=False,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p2 = subprocess.Popen(('ps2ascii', '-'),
                          shell=False,
                          stdin=p1.stdout,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout2, stderr2 = p2.communicate()
    if not (p2.returncode != 0 or stderr2):
        return stdout2.decode()


def pdfconverter_viahtml(pdf_filename):
    """Try to convert the given pdf file it text using conversion to HTML
    and then HTML to text. This is the only way to get anything worthy from 
    Ameritrade confirmation statements.

    Args:
      pdf_filename: the name of the PDF file to convert.
    Returns:
      A string of the converted text, if successful, or None, if not.
    """
    p1 = subprocess.Popen(('pdftohtml', '-i', '-stdout', pdf_filename),
                          shell=False,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p2 = subprocess.Popen(('html2text',),
                          shell=False,
                          stdin=p1.stdout,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout2, stderr2 = p2.communicate()
    if not (p2.returncode != 0 or stderr2):
        return stdout2.decode()


PDF_CONVERTERS = [
    pdfconverter_poppler_pdftotext,
    pdfconverter_unoconv_plus_pdftotext,
    pdfconverter_ghostscript,
    pdfconverter_viahtml,
    ]
