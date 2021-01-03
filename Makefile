EMACS = $(PWD)/result/bin/emacs
BUILD = .make/build

.PHONY: build
build: $(BUILD)

$(BUILD): $(shell find . -type f -name '*.nix')
	@mkdir -p .make
	nix-build
	@touch $(BUILD)

.PHONY: run
run : $(BUILD)
	@$(EMACS) -q -l early-init.el -l init.el --eval "(run-hooks 'after-init-hook)"

.PHONY: check
check: $(BUILD)
	@$(EMACS) --batch \
		-l shut-up -f shut-up-silence-emacs \
		-l config-tests.el -l early-init.el -l init.el \
		-f ert-run-tests-batch-and-exit
