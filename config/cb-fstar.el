;;; cb-fstar.el --- Configuration for the F* language.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil)
(require 'spacemacs-keys)


(use-package fstar-mode
  :config
  (progn
    (setq fstar-enabled-modules (delq 'prettify fstar-enabled-modules))
    (evil-define-key 'normal fstar-mode-map (kbd "M-.") 'fstar-jump-to-definition)
    (evil-define-key 'insert fstar-mode-map (kbd "M-.") 'fstar-jump-to-definition)
    (evil-define-key 'normal fstar-mode-map (kbd "K") 'fstar-doc-at-point-dwim)

    (spacemacs-keys-set-leader-keys-for-major-mode 'fstar-mode
      "W" 'fstar-browse-wiki-in-browser
      "d" 'fstar-visit-dependency
      "ec" 'fstar-eval-custom
      "ee" 'fstar-eval
      "h" 'fstar-doc
      "l" 'fstar-outline
      "o" 'fstar-list-options
      "p" 'fstar-print
      "q" 'fstar-quit-windows
      "r" 'fstar-subp-reload-to-point
      "s" 'fstar-insert-match-dwim
      "v" 'fstar-cli-verify
      "SPC" 'fstar-subp-advance-or-retract-to-point
      "w" 'fstar-browse-wiki
      "/" 'fstar-search)))


(provide 'cb-fstar)

;;; cb-fstar.el ends here
