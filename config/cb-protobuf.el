;;; cb-protobuf.el --- Configuration for protobuf -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'cb-protobuf)

;;; cb-protobuf.el ends here
