;;; cb-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

;; Colours taken from the Solarized palette.

(defconst cb-theme-common-yellow "#b58900")
(defconst cb-theme-common-orange "#cb4b16")
(defconst cb-theme-common-red "#dc322f")
(defconst cb-theme-common-magenta "#d33682")
(defconst cb-theme-common-violet "#6c71c4")
(defconst cb-theme-common-blue "#268bd2")
(defconst cb-theme-common-cyan "#2aa198")
(defconst cb-theme-common-green "#859900")

(defun cb-theme-common-make-theme (default-foreground default-background)
  `((default
      ((t
        (:background ,default-background :foreground ,default-foreground :weight normal :family "Hasklig" :height 130))))

    (fringe
     ((t
       (:background ,default-background))))

    ;; General font-lock faces.

    (font-lock-keyword-face
     ((t (:weight light))))

    (font-lock-builtin-face
     ((t (:weight light))))

    (font-lock-variable-name-face
     ((t (:weight normal))))

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

    (org-document-info-keyword
     ((t (:weight light))))

    (org-date
     ((t (:weight light))))
    (org-sexp-date
     ((t (:weight light))))

    (org-date
     ((t (:underline t))))

    (org-agenda-date-today
     ((t (:underline ,cb-theme-common-magenta :weight semibold))))

    (org-formula
     ((t (:weight light))))

    (org-tag
     ((t (:weight light))))

    (org-table
     ((t (:inherit default))))

    (org-scheduled
     ((t (:inherit default))))

    (org-meta-line
     ((t (:weight light))))

    (org-agenda-structure
     ((t (:weight demibold))))

    (org-document-title
     ((t (:weight bold))))

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
     ((t (:inherit outline-1))))

    ;; Ledger

    (ledger-font-comment-face
     ((t (:weight light))))

    (ledger-font-posting-date-face
     ((t (:inherit default))))

    (ledger-font-posting-account-face
     ((t (:inherit default))))

    (ledger-font-xact-highlight-face
     ((t nil)))

    (ledger-font-other-face
     ((t (:inherit default :weight demibold))))

    (ledger-font-directive-face
     ((t (:inherit default))))

    (ledger-font-posting-amount-face
     ((t (:inherit default))))


    ;; Misc faces

    (parenthesis
     ((t (:weight light))))

    (link
     ((t (:inherit default :underline t))))))

(provide 'cb-theme-common)

;;; cb-theme-common.el ends here
