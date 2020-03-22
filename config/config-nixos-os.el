;;; config-nixos-os.el --- Configuration for NixOS hosts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'server)

(use-package exec-path-from-shell
  :straight t
  :init
  (defconst exec-path-from-shell-variables
    '("PATH"
      "LEDGER_FILE"
      "GPG_TTY"
      "PASSWORD_STORE_DIR"
      "SSH_AUTH_SOCK"
      "SSH_KEY_PATH"))
  :config
  (exec-path-from-shell-initialize))

(set-fontset-font t 'arabic "amiri")

(unless (server-running-p)
  (server-start))

(provide 'config-nixos-os)

;;; config-nixos-os.el ends here
