;;; messages-are-flowing-hacks.el --- Hacks for messages-are-flowing  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature 'messages-are-flowing)

(with-eval-after-load 'messages-are-flowing
  (el-patch-defun messages-are-flowing-use-and-mark-hard-newlines ()
    "Turn on `use-hard-newlines', and make hard newlines visible.
The main use of this is to send \"flowed\" email messages, where
line breaks within paragraphs are adjusted by the recipient's
device, such that messages remain readable on narrow displays."
    (interactive)
    (use-hard-newlines (el-patch-add +1 t))
    (add-hook 'after-change-functions 'messages-are-flowing--mark-hard-newlines nil t)))

(provide 'messages-are-flowing-hacks)

;;; messages-are-flowing-hacks.el ends here
