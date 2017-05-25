;;; cb-latex.el --- Configuration for latex.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package latex-preview-pane
  :commands (latex-preview-pane-enable))


(provide 'cb-latex)

;;; cb-latex.el ends here
