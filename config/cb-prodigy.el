;;; cb-prodigy.el --- Configuration for the prodigy process manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evilified-state)
(require 'spacemacs-keys)

(with-eval-after-load 'which-key
  (with-no-warnings
    (push `((nil . ,(rx bos "prodigy-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package prodigy
  :commands (prodigy)
  :init
  (spacemacs-keys-set-leader-keys "as" #'prodigy)
  :config
  (progn
    (evil-set-initial-state 'prodigy-mode 'motion)
    (evilified-state-evilify-map prodigy-mode-map
      "h" 'prodigy-first
      "j" 'prodigy-next
      "k" 'prodigy-prev
      "l" 'prodigy-last
      "H" 'prodigy-display-process
      "J" 'prodigy-next-with-status
      "K" 'prodigy-prev-with-status
      "L" 'prodigy-start
      "d" 'prodigy-jump-dired
      "g" 'prodigy-jump-magit
      "Y" 'prodigy-copy-cmd
      "R" 'revert-buffer
      "q" 'quit-buffer)
    (evil-define-key 'motion prodigy-view-mode-map (kbd "gf") 'find-file-at-point)))


(provide 'cb-prodigy)

;;; cb-prodigy.el ends here
