emacs-batch = emacs --batch -l $(PWD)/early-init.el -l $(PWD)/init.el

.PHONY: check
check: check-loads-config


.PHONY: check-loads-config
check-loads-config:
	@echo -n "config.el loading... "
	@if $(emacs-batch) 2>&1 | grep 'config.el loaded' > /dev/null; then echo "OK"; else echo "FAIL"; fi
