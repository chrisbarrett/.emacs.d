EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
CASK         = cask
CASKFILE     = ./Cask
CASKDIR      = ./.cask
SRCS         = ./org-drill-table.el
TESTS        = ./test/org-drill-table-tests.el
OBJECTS      = $(SRCS:.el=.elc)


.PHONY: compile clean clean-all test


compile : $(OBJECTS)


test : compile
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit


clean :
	rm -f $(OBJECTS)


clean-all : clean
	rm -rf $(CASKDIR)


%.elc : %.el $(CASKDIR)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(CASKDIR)")' -f package-initialize \
	-f batch-byte-compile $<


$(CASKDIR) : $(CASKFILE)
	$(CASK)
	$(CASK) install
	touch $(CASKDIR)
