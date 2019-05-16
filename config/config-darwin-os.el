;;; config-darwin-os.el --- OSX-specific configuration.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)

(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'copy-region-as-kill)
(global-set-key (kbd "s-w") 'delete-frame)
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-q") 'delete-frame)

(global-unset-key (kbd "<f11>"))
(global-set-key (kbd "<s-return>") #'toggle-frame-fullscreen)

(set-frame-parameter (selected-frame) 'alpha '(97 80))
(add-to-list 'default-frame-alist '(alpha 97 80))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . nil))

;; Never show a proxy icon in the title bar.

(setq ns-use-proxy-icon nil)

;; Graphical applications in macOS inherit their process environment from
;; launchd, not from a shell process which loads a profile.

(use-package exec-path-from-shell
  :straight t
  :preface
  (declare-function exec-path-from-shell-initialize "config-darwin-os")
  :init
  (progn
    (defvar exec-path-from-shell-arguments '("-l"))

    (defconst exec-path-from-shell-variables
      '("PATH"
        "MANPATH"
        "NIX_REMOTE"
        "NIX_USER_PROFILE_DIR"
        "NIX_PROFILES"
        "NIX_SSL_CERT_FILE"
        "NIX_PATH"
        "SSH_AGENT_PID"
        "GPG_TTY"
        "TEXINPUTS"
        "RUST_SRC_PATH")))
  :config
  (progn
    (exec-path-from-shell-initialize)
    (add-to-list 'exec-path "~/.local/bin")

    ;; Use gnu coreutils ls command, if available.
    (when-let* ((gls (executable-find "gls")))
      (setq insert-directory-program gls))))

(provide 'config-darwin-os)

;;; config-darwin-os.el ends here
