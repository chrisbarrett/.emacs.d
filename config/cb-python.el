;;; cb-python.el --- Configuration for python.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package python
  :defer t
  :config
  (progn
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    (defalias 'python-indent-dedent-line-backspace #'sp-backward-delete-char)))


(provide 'cb-python)

;;; cb-python.el ends here
