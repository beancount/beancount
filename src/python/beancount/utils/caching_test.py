import unittest

from beancount.utils import caching


# class TestCaching(unittest.TestCase):

#     def test_caching(self):

def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser()

    for ctype in 'load', 'price', 'convert':
        cache = caching.Cache(ctype)
        cache.add_args(parser)
    args = parser.parse_args()
    cache.configure(args)




if __name__ == '__main__':
    main()


"""
  --convert-cache-dir DIR
  --convert-cache-disable, --no-convert-cache
  --convert-cache-clear, --clear-convert-cache

  --convert-cache=<filename>,DISABLE|OFF,CLEAR
  --convert-cache=DISABLE, --no-convert-cache
  --convert-cache=CLEAR, --clear-convert-cache

"""
