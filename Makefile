EMACS = $(PWD)/result/bin/emacs
BUILD = .make/build

.PHONY: build
build: $(BUILD)

$(BUILD): $(shell find . -type f -name '*.nix')
	@mkdir -p .make
	nix-build
	@touch $(BUILD)

.PHONY: check
check: $(BUILD)
	@$(EMACS) --batch \
		-l config-tests.el -l early-init.el -l init.el \
		-f ert-run-tests-batch-and-exit
