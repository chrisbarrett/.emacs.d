EMACS ?= emacs
CASK ?= cask

all: test

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile git.el

clean-elc:
	rm -f git.elc

.PHONY:	all test docs
