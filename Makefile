LISP = $(PWD)/lisp

ORG_D = $(LISP)/org-mode
ORG_LOADDEFS = $(ORG_D)/lisp/org-loaddefs.el

TARGETS = $(ORG_LOADDEFS)


.PHONY: all clean


all : $(TARGETS)


clean :
	$(MAKE) -C $(ORG_D) clean


$(ORG_LOADDEFS) :
	$(MAKE) -C $(ORG_D)
