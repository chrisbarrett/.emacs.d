;;; cb-manpages.el --- Configuration for manpage utils.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package man
  :defer t
  :commands (Man-next-section
             Man-previous-section)
  :config
  (progn
    (define-key Man-mode-map (kbd "M-n") #'Man-next-section)
    (define-key Man-mode-map (kbd "M-p") #'Man-previous-section)))

(provide 'cb-manpages)

;;; cb-manpages.el ends here
