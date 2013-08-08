#!/bin/bash
# A utility script to install dependencies from source.
# We provide this script as a convenience because Python 3.3 is not yet widely distributed with most distributions.
# Run this once to install a newly build Python interpreter with all the required dependencies.
# Tested on Linux (possibly will work on Mac OS X too).

BEANCOUNT=$(dirname $0)

SRC=/tmp/src
mkdir -p $SRC

# Install Python3
if ! python3 --version 2>&1 | grep -q 3.3 ; then
  cd $SRC
  wget -nc http://python.org/ftp/python/3.3.2/Python-3.3.2.tgz
  tar zxf Python-3.3.2.tgz
  cd Python-3.3.2 && ./configure && make && sudo make install
fi
PYTHON=$(type -p python3)

function module_exists()
{
  $PYTHON -c "import $1" 2>/dev/null
}


# Install cdecimal
if ! module_exists "cdecimal" ; then
  cd $SRC
  wget -nc http://www.bytereef.org/software/mpdecimal/releases/cdecimal-2.3.tar.gz
  tar zxf cdecimal-2.3.tar.gz
  cd cdecimal-2.3 && sudo $PYTHON setup.py install
fi

# Install bottle
if ! module_exists "bottle" ; then
  cd $SRC
  git clone https://github.com/defnull/bottle
  cd bottle && sudo $PYTHON setup.py install
fi

# Install BeautifulSoup
if ! module_exists "bs4" ; then
  cd $SRC
  wget -nc http://www.crummy.com/software/BeautifulSoup/bs4/download/4.2/beautifulsoup4-4.2.1.tar.gz
  tar zxf beautifulsoup4-4.2.1.tar.gz
  cd beautifulsoup4-4.2.1 && sudo $PYTHON setup.py install
fi

# Build beancount itself
cd $BEANCOUNT
make build

echo "Installation complete. Run examples/rundemo.sh"
