;;; cb-tiny.el --- Configuration for tiny  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tiny
  :bind ("C-:" . tiny-expand))

(provide 'cb-tiny)

;;; cb-tiny.el ends here
