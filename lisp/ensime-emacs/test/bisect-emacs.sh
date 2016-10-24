#!/bin/bash

# deps:
#  - build-deps emacs
#  - xvfb
#  - oracle-6-jdk

# This file is to help us track upstream regressions in emacs. On
# debian systems, type `apt-get build-deps emacs` to get everything
# you need to build emacs. Then from the root of an emacs git repo,
# do a typical git bisect pointing at this script in the run stage.

export TEST_DIR=$PWD/../ensime-emacs
export EMACS=$PWD/src/emacs

git clean -xfd

./autogen.sh
# unfortunately there are some build failures in the emacs using --without-all
#./configure --without-all --cache-file=/tmp/config.cache
./configure --cache-file=/tmp/config.cache
make -C lib all || exit 125
make -C src bootstrap-emacs || exit 125
make -C src emacs || exit 125

cd $TEST_DIR

`dirname $0`/run_emacs_tests.sh

RET=$?

killall Xvfb

exit $RET
