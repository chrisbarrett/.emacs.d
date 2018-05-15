;;; cb-major-mode-hydra.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;;; Commentary:

;;; Code:

(require 'pretty-hydra)
(autoload 'all-the-icons-icon-for-mode "all-the-icons")

(defun cb-major-mode-hydra--prepend-major-mode-header (mode)
  (lambda (str)
    (format "%s %s\n%s" (all-the-icons-icon-for-mode mode) mode str)))

(cl-defmacro cb-major-mode-hydra-define (mode &rest heads-plist)
  (declare (indent defun) (doc-string 3))
  (let ((name (intern (format "major-mode-hydra--%s" mode)))
        (with-header (cb-major-mode-hydra--prepend-major-mode-header mode)))
    `(pretty-hydra-define ,name (:color teal :hint nil)
       ,heads-plist
       :docstring-formatter ,with-header)))

(defun cb-major-mode-hydra ()
  "Show the hydra for the current-buffer's major mode."
  (interactive)
  (let ((fname (intern (format "major-mode-hydra--%s/body" major-mode))))
    (if (fboundp fname)
        (call-interactively fname)
      (user-error "No major mode hydra for %s" major-mode))))

(provide 'cb-major-mode-hydra)

;;; cb-major-mode-hydra.el ends here
