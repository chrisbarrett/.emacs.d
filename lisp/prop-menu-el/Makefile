EMACS=emacs24

BATCHEMACS=$(EMACS) --batch --no-site-file -q -eval '(add-to-list (quote load-path) "${PWD}/")' -eval '(require (quote package))' -eval '(add-to-list (quote package-archives) (quote ("melpa" . "http://melpa.org/packages/")) t)' -eval '(package-initialize)'

BYTECOMP = $(BATCHEMACS) -eval '(progn (require (quote bytecomp)) (setq byte-compile-warnings t) (setq byte-compile-error-on-warn t))' -f batch-byte-compile

OBJS = prop-menu.elc

.el.elc:
	$(BYTECOMP) $<

build: $(OBJS)

test:
	$(BATCHEMACS) -l ert -l prop-menu-tests.el -f ert-run-tests-batch-and-exit

clean:
	-rm -f $(OBJS)

getdeps:
	$(BATCHEMACS) -eval '(progn (package-refresh-contents))'

.PHONY: clean build test
