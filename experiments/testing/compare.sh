#!/bin/bash
# Compare a particular file regurgitated by Beancount between a few branches.
BRANCHES="default booking"
export BEANCOUNT_DISABLE_LOAD_CACHE=1
FILES=""
for BRANCH in ${BRANCHES} ; do
    hg update -C ${BRANCH}
    make clean build
    bean-query $L print > /tmp/${BRANCH}.beancount
    FILES="$FILES /tmp/${BRANCH}.beancount"
done
xxdiff $FILES
