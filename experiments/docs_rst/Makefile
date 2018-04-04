#!/usr/bin/env make

# Directory where the downloaded files are located (either downloaded using the
# Google Drive API or decompressed from the provided archive).
CONVERT_DOCS=$(HOME)/docs

# Directory of Dominik Aumayr's static docs generator source.
RST_DOCS=$(HOME)/p/beancount-docs

# Download all the docs.
download:
	mkdir -p $(CONVERT_DOCS)
	./download_docs.py $(CONVERT_DOCS)

# Convert all the docs.
convert:
	./convert_docs.py $(CONVERT_DOCS)

# Copy the docs to the static archive.
# (You can then go and build then using Sphinx and inspect them.)
copy:
	./copy_docs.py $(CONVERT_DOCS) $(RST_DOCS)
