;;; init.el --- User init file for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(defmacro use-config (feature &rest use-package-args)
  (declare (indent 1))
  (cl-assert (file-exists-p (expand-file-name (format "./config/%s.el" feature)
                                              user-emacs-directory)))
  `(use-package ,feature
     :load-path "./config/" :demand t ,@use-package-args))

(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(rx "("
                                (group "use-config") symbol-end (* (any space))
                                (group (+ (or (syntax word) (syntax symbol))))
                                (? ")"))
                           (1 font-lock-keyword-face)
                           (2 font-lock-constant-face nil t))))

;;; init.el ends here
