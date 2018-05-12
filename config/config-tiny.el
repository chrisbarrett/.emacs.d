;;; config-tiny.el --- Configuration for tiny  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package tiny
  :straight t
  :bind ("C-:" . tiny-expand))

(provide 'config-tiny)

;;; config-tiny.el ends here
