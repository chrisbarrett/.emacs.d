;;; cb-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defun cb-theme-common-make-theme (default-foreground default-background)
  `((default
      ((t
        (:background ,default-background :foreground ,default-foreground :weight normal :family "Hasklig" :height 130))))


    (link
     ((t (:inherit default :underline t))))

    ;; General font-lock faces.

    (font-lock-keyword-face
     ((t (:weight light))))

    (font-lock-builtin-face
     ((t (:weight light))))

    (font-lock-variable-name-face
     ((t (:weight demibold))))

    (font-lock-function-name-face
     ((t (:weight demibold))))

    (font-lock-constant-face
     ((t (:weight normal))))

    (font-lock-type-face
     ((t (:weight normal))))

    (font-lock-type-name-face
     ((t (:weight normal))))

    (font-lock-string-face
     ((t (:weight light))))

    (font-lock-comment-face
     ((t (:weight demibold))))

    ;; Magit

    (magit-diff-file-heading
     ((t (:weight normal))))

    (magit-section-heading
     ((t (:weight demibold))))

    ;; Outline, Org

    (org-done
     ((t (:inherit default :weight bold :foreground "grey60"))))

    (org-date
     ((t (:weight light))))
    (org-sexp-date
     ((t (:weight light))))

    (org-formula
     ((t (:weight light))))

    (org-tag
     ((t (:weight light))))

    (org-table
     ((t (:inherit default))))

    (org-meta-line
     ((t (:weight light))))

    (org-agenda-structure
     ((t (:weight demibold))))

    (outline-1
     ((t (:weight demibold))))
    (outline-2
     ((t (:inherit outline-1))))
    (outline-3
     ((t (:inherit outline-1))))
    (outline-4
     ((t (:inherit outline-1))))
    (outline-5
     ((t (:inherit outline-1))))
    (outline-6
     ((t (:inherit outline-1))))
    (outline-7
     ((t (:inherit outline-1))))
    (outline-8
     ((t (:inherit outline-1))))
    (outline-9
     ((t (:inherit outline-1))))))

(provide 'cb-theme-common)

;;; cb-theme-common.el ends here
