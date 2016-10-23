;;; cb-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package flycheck
  :defer 1
  :commands (global-flycheck-mode)

  :config
  (progn
    (global-flycheck-mode +1)

    (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
    (setq flycheck-display-errors-delay 0.5)

    (spacemacs-keys-declare-prefix "e" "errors")

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
      (evil-define-key 'normal flycheck-mode-map
        "M-n" #'flycheck-next-error
        "M-p" #'flycheck-previous-error))))


(provide 'cb-flycheck)

;;; cb-flycheck.el ends here
