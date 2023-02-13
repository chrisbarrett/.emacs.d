EMACS = $(PWD)/result/bin/emacs
BUILD = .make/build
TARGET_EL = config-autoloads.el config.el
NIX = /run/current-system/sw/bin/nix

TARGETS := $(BUILD) $(TARGET_EL)
NIX_SRCS := flake.lock $(shell find . -type f -name '*.nix')
SRCS := config.org $(NIX_SRCS)

EMACS_RUN_OPTS = -l early-init.el -l init.el --eval "(run-hooks 'after-init-hook)"

.PHONY: build
build: $(TARGETS)

$(BUILD): $(NIX_SRCS)
	@mkdir -p .make
	$(NIX) build
	@touch $(BUILD)

$(TARGET_EL) : $(BUILD) $(SRCS)
	@rm -f $(TARGET_EL)
	@NIX_EMACS_BUILDING_CONFIG_P=1 $(EMACS) --batch -l shut-up -f shut-up-silence-emacs \
		--eval '(setq make-backup-files nil)' \
		--eval '(setq-default indent-tabs-mode nil)' \
		-l early-init.el -l init.el

.PHONY: clean
clean :
	rm -f $(TARGET_EL)

.PHONY: run
run : $(TARGETS)
	@$(EMACS) -q $(EMACS_RUN_OPTS)

.PHONY: check
check: $(TARGETS)
	@$(EMACS) --batch \
		-l shut-up -f shut-up-silence-emacs -l config-tests.el $(EMACS_RUN_OPTS) \
		-f ert-run-tests-batch-and-exit
