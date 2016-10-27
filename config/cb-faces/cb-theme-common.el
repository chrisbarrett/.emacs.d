;;; cb-theme-common.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defun cb-theme-common-make-theme (default-foreground default-background)
  `((default
      ((t
        (:background ,default-background :foreground ,default-foreground :weight normal :family "Hasklig" :height 130))))

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
     ((t (:weight demibold))))))

(provide 'cb-theme-common)

;;; cb-theme-common.el ends here
