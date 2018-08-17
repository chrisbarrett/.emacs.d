;;; config-eshell.el --- Configuration for eshell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'f)

(defconst config-eshell-etc-directory (f-join paths-etc-directory "eshell"))

(use-package eshell
  :commands (eshell)
  :config
  (progn
    (f-mkdir config-eshell-etc-directory)
    (general-setq eshell-aliases-file (f-join config-eshell-etc-directory  "aliases"))))

(provide 'config-eshell)

;;; config-eshell.el ends here
