emacs-batch = emacs --batch -l $(PWD)/early-init.el -l $(PWD)/init.el

.PHONY: check
check: check-loads-config check-startup-duration


.PHONY: check-loads-config
check-loads-config:
	@echo -n "check: config.el loads... "
	@if $(emacs-batch) 2>&1 | grep 'config.el loaded' > /dev/null; then echo "OK"; else echo "FAIL"; fi

.PHONY: check-startup-duration
check-startup-duration:
	@echo -n "check: config.el loads in less than 1 s... "
	@if test 0 -eq "$$($(emacs-batch) --eval '(message "%s" (floor emacs-init-duration))' 2>&1 | tail -n 1)"; then echo "OK"; else echo "FAIL"; fi
