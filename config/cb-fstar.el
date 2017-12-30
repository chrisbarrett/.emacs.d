;;; cb-fstar.el --- Configuration for the F* language.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil)
(require 'spacemacs-keys)

(use-package which-key
  :config
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'fstar-mode "m e" "eval")

    (push `((nil . ,(rx bos "fstar-" (? "subp-") (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package fstar-mode
  :mode ("\\.fsti?\\'" . fstar-mode)
  :config
  (progn
    (setq fstar-enabled-modules
          (seq-difference fstar-enabled-modules '(prettify
                                                  overlay-legend
                                                  spinner)))

    (setq fstar-auto-insert--skeleton
          '(nil "module " (fstar-auto-insert--infer-module-name) "\n"))

    (setq fstar-auto-insert--alist-form
          `(("\\.fsti?\\'" . "F* program") . ,fstar-auto-insert--skeleton))

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
