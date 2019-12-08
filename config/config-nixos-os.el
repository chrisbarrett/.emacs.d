;;; config-nixos-os.el --- Configuration for NixOS hosts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :straight t
  :init
  (defconst exec-path-from-shell-variables
    '("LEDGER_FILE"
      "GPG_TTY"
      "SSH_AUTH_SOCK"
      "SSH_KEY_PATH"))
  :config
  (exec-path-from-shell-initialize))

(set-fontset-font t 'arabic "amiri")

(provide 'config-nixos-os)

;;; config-nixos-os.el ends here
