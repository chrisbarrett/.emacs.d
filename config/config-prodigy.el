;;; config-prodigy.el --- Configuration for the prodigy process manager.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package prodigy
  :straight t
  :commands (prodigy)
  :general
  (:states 'motion :keymaps 'prodigy-view-mode-map "gf" 'find-file-at-point)
  :general
  (:states 'motion :keymap 'prodigy-mode-map
   "h" #'prodigy-first
   "j" #'prodigy-next
   "k" #'prodigy-prev
   "l" #'prodigy-last
   "H" #'prodigy-display-process
   "J" #'prodigy-next-with-status
   "K" #'prodigy-prev-with-status
   "L" #'prodigy-start
   "d" #'prodigy-jump-dired
   "g" #'prodigy-jump-magit
   "Y" #'prodigy-copy-cmd
   "R" #'revert-buffer
   "q" #'quit-buffer))

(provide 'config-prodigy)

;;; config-prodigy.el ends here
