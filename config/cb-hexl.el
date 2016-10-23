;;; cb-hexl.el --- Configuration for hexl.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(autoload 'evil-define-key "evil-core")

(use-package hexl
  :defer t

  :init
  (progn
    (spacemacs-keys-set-leader-keys "fh" 'hexl-find-file)
    (spacemacs-keys-set-leader-keys-for-major-mode 'hexl-mode
      "d" 'hexl-insert-decimal-char
      "c" 'hexl-insert-octal-char
      "x" 'hexl-insert-hex-char
      "X" 'hexl-insert-hex-string
      "g" 'hexl-goto-address))

  :config
  (evil-define-key 'motion hexl-mode-map
    "]]" 'hexl-end-of-1k-page
    "[[" 'hexl-beginning-of-1k-page
    "h" 'hexl-backward-char
    "l" 'hexl-forward-char
    "j" 'hexl-next-line
    "k" 'hexl-previous-line
    "$" 'hexl-end-of-line
    "^" 'hexl-beginning-of-line
    "0" 'hexl-beginning-of-line))


(provide 'cb-hexl)

;;; cb-hexl.el ends here
