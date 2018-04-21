CASK ?= cask

CASK_D = .cask



.PHONY : install test clean


install : $(CASK_D)


test : $(CASK_D)
	$(CASK) exec ert-runner


clean :
	rm -rf .cask
	rm *.elc



$(CASK_D) : ./Cask
	cask install
