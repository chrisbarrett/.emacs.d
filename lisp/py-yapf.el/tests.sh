#!/bin/bash -e

TEST_FILE=/tmp/py-test-file.py


install_emacs24() {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y
    sudo apt-get install emacs24 -y
}


test_install_package() {
    echo $FUNCNAME
    emacs -nw py-yapf.el \
          -f package-install-from-buffer \
          -f kill-emacs
}


test_01() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests.el \
          --load py-yapf.el \
          ./tests/01/before.py \
          -f py-yapf-buffer \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/01/after.py
}


test_02() {
    echo $FUNCNAME
    rm $TEST_FILE || true
    emacs --no-init-file -nw \
          --load ./tests.el \
          --load ./tests/02/init.el \
          --load py-yapf.el \
          ./tests/02/before.py \
          -f write-test-file \
          -f kill-emacs

    diff $TEST_FILE ./tests/02/after.py
}


main() {
    if [ "$TRAVIS" = "true" ]; then
        install_emacs24
        test_install_package
    fi

    test_01
    test_02
}


main
