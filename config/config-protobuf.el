;;; config-protobuf.el --- Configuration for protobuf -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'config-protobuf)

;;; config-protobuf.el ends here
