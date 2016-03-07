"""Example importer for example broker UTrade.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

from beancount.ingest import importer
from beancount.ingest import regression


class Importer(importer.ImporterProtocol):
    pass



def test():
    importer = Importer()
    yield from regression.compare_sample_files(importer, __file__)
