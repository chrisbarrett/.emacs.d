;;; cb-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package flycheck
  :defer 1
  :commands (global-flycheck-mode)

  :init
  (spacemacs-keys-declare-prefix "e" "errors")

  :config
  (progn
    (global-flycheck-mode +1)

    (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)

    (setq flycheck-global-modes
          '(not sbt-file-mode))

    (spacemacs-keys-set-leader-keys
      "ec" #'flycheck-clear
      "eh" #'flycheck-describe-checker
      "el" #'flycheck-list-errors
      "ee" #'flycheck-explain-error-at-point
      "en" #'flycheck-next-error
      "eN" #'flycheck-next-error
      "ep" #'flycheck-previous-error
      "es" #'flycheck-select-checker
      "eS" #'flycheck-set-checker-executable
      "ev" #'flycheck-verify-setup)

    (with-eval-after-load 'evil
      (bind-key "M-n" #'flycheck-next-error flycheck-mode-map)
      (bind-key "M-p" #'flycheck-previous-error flycheck-mode-map)))

  :functions
  (flycheck-clear
   flycheck-describe-checker
   flycheck-list-errors
   flycheck-explain-error-at-point
   flycheck-next-error
   flycheck-next-error
   flycheck-previous-error
   flycheck-select-checker
   flycheck-set-checker-executable
   flycheck-verify-setup))


(provide 'cb-flycheck)

;;; cb-flycheck.el ends here
