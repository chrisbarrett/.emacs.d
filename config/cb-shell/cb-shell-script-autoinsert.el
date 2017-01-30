;;; cb-shell-script-autoinsert.el --- Auto-insert features for shell scripts.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'f-ext? "f")
(autoload 'yas-expand-snippet "yasnippet")

(defconst cb-shell-script-autoinsert-form
  '((sh-mode . "Shell Script") . cb-shell-script-autoinsert--template-string))

(defun  cb-shell-script-autoinsert--template-string ()
  (let ((program
         (if (f-ext? (buffer-name) "zsh")
             "zsh"
           "bash")))

    (yas-expand-snippet (format "#!/usr/bin/env %s

$0
" program))))

(provide 'cb-shell-script-autoinsert)

;;; cb-shell-script-autoinsert.el ends here
