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
	@./scripts/test.sh "$(EMACS)"
