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
    (evil-define-key 'normal fstar-mode-map (kbd "M-.") 'fstar-jump-to-definition)
    (evil-define-key 'insert fstar-mode-map (kbd "M-.") 'fstar-jump-to-definition)
    (evil-define-key 'normal fstar-mode-map (kbd "K") 'fstar-doc-at-point-dwim)

    (spacemacs-keys-set-leader-keys-for-major-mode 'fstar-mode
      "E" 'fstar-eval-custom
      "W" 'fstar-browse-wiki-in-browser
      "d" 'fstar-visit-dependency
      "e" 'fstar-eval
      "h" 'fstar-doc
      "l" 'fstar-outline
      "o" 'fstar-list-options
      "p" 'fstar-print
      "q" 'fstar-quit-windows
      "s" 'fstar-insert-match-dwim
      "v" 'fstar-cli-verify
      "w" 'fstar-browse-wiki
      "/" 'fstar-search)))


(provide 'cb-fstar)

;;; cb-fstar.el ends here
