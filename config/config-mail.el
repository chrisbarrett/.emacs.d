;;; config-mail.el --- Configuration for mail client software.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)

;; `mu4e' is an Emacs mail client. I install the lisp along with the 'mu'
;; program via Nix.

(add-to-list 'load-path (f-join paths-site-lisp-directory "mu4e"))

(use-package mu4e
  :commands (mu4e)
  :custom
  ((mu4e-compose-format-flowed t)
   (message-kill-buffer-on-exit t)))


;; `messages-are-flowing' displays newline symbols in the buffer for hard newlines.

(use-package messages-are-flowing
  :straight t
  :hook (message-mode . messages-are-flowing-use-and-mark-hard-newlines))

(provide 'config-mail)

;;; config-mail.el ends here
