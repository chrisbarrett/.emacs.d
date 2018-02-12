;;; cb-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)

(defconst cb-theme-common-yellow "#b58900")
(defconst cb-theme-common-orange "#cb4b16")
(defconst cb-theme-common-red "red1")
(defconst cb-theme-common-pink "pink")
(defconst cb-theme-common-magenta "#d33682")
(defconst cb-theme-common-blue "#268bd2")
(defconst cb-theme-common-cyan "#2aa198")
(defconst cb-theme-common-green "#859900")
(defconst cb-theme-common-yellow "#b58900")

(defconst cb-theme-common-font-family
  (--first (find-font (font-spec :name it))
           '("Hasklig" "DejaVu Sans Mono" "Menlo")))

;; TODO: Maybe move this into a host-specific way of setting config.
(defconst cb-theme-common-default-height
  (pcase system-type
    (`darwin
     145)
    (_
     120)))

(cl-defun cb-theme-common-make-theme (&key
                       default-fg
                       default-bg
                       region-bg
                       header-line-bg
                       subtle-bg
                       highlight-bg
                       paren-match
                       emphasis-1
                       emphasis-2
                       (header-line-fg default-bg)
                       (mid-grey "grey60")
                       (dark-grey "grey40")
                       (light-grey "grey80")
                       (error-fg cb-theme-common-red)
                       (dimmed-fg mid-grey)
                       (pending-bg "#AAAA33"))
  `((default
      ((t
        :background ,default-bg
        :foreground ,default-fg
        :weight normal
        :family ,cb-theme-common-font-family
        :height ,cb-theme-common-default-height)))

    (fixed-pitch
     ((t :family ,cb-theme-common-font-family)))

    (success
     ((t :weight normal :foreground ,cb-theme-common-green)))
    (warning
     ((t :weight normal :foreground ,cb-theme-common-orange)))
    (error
     ((t :weight normal :foreground ,error-fg)))

    (link
     ((((background light))
       :weight light :underline ,light-grey)
      (((background dark))
       :weight light :underline ,dark-grey)))

    (fringe
     ((t :background ,default-bg)))

    (header-line
     ((t
       :background ,header-line-bg
       :foreground ,header-line-fg :weight demibold
       :box (:line-width 3 :color ,header-line-bg))))

    (cb-header-line-format-nonemphased-element
     ((t :weight light)))

    (mode-line
     ((t :inherit header-line
         :foreground ,header-line-bg
         :box (:line-width 1 :color ,header-line-bg)
         :height 0.1)))

    (mode-line-inactive
     ((t :inherit header-line :height 20)))

    (evil-transient-state-title-face
     ((t :inherit header-line :background ,header-line-bg)))

    (region
     ((t :background ,region-bg)))

    (hl-line
     ((t :background ,subtle-bg)))

    ;; General font-lock faces.

    (font-lock-keyword-face
     ((t :weight light)))

    (font-lock-builtin-face
     ((t :weight light)))

    (font-lock-variable-name-face
     ((t :weight normal)))

    (font-lock-function-name-face
     ((t :weight demibold)))

    (font-lock-constant-face
     ((t :weight normal)))

    (font-lock-type-face
     ((t :weight normal)))

    (font-lock-type-name-face
     ((t :weight normal)))

    (font-lock-string-face
     ((t :weight light)))

    (font-lock-comment-face
     ((t :weight demibold)))

    ;; Info faces

    (info-xref
     ((t :inherit link :weight normal)))

    (info-xref-visited
     ((t :inherit link-visited :weight normal)))

    (info-string
     ((t :inherit font-lock-string-face)))

    (info-node
     ((t :weight demibold)))

    (info-reference-item
     ((t :weight demibold)))

    (info-function-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-macro-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-command-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-special-form-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-syntax-class-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-user-option-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    (info-variable-ref-item
     ((t :weight demibold :foreground ,mid-grey)))

    ;; Dired

    (dired-header
     ((t :weight bold)))

    (diredp-dir-heading
     ((t :inherit dired-header)))

    (diredp-dir-name
     ((t :foreground ,emphasis-1)))

    (diredp-file-name
     ((t :inherit default)))

    (diredp-ignored-file-name
     ((t :inherit diredp-file-name :foreground ,dimmed-fg)))

    (diredp-symlink
     ((t :inherit diredp-file-name :foreground ,mid-grey)))

    (diredp-file-suffix
     ((t :foreground ,dimmed-fg)))

    (diredp-compressed-file-suffix
     ((t :inherit diredp-file-suffix)))

    (diredp-number
     ((t :weight light)))

    (diredp-date-time
     ((t :foreground ,mid-grey :weight light)))

    (diredp-dir-priv
     ((t :weight light :foreground ,mid-grey)))

    (diredp-no-priv
     ((t :weight light :foreground ,mid-grey)))

    (diredp-rare-priv
     ((t :weight light :foreground ,mid-grey)))

    (diredp-exec-priv
     ((t :weight light :foreground ,mid-grey)))

    (diredp-read-priv
     ((t :weight light :foreground ,mid-grey)))

    (diredp-write-priv
     ((t :weight light :foreground ,mid-grey)))

    ;; Info

    (info-double-quoted-name
     ((t :weight normal)))

    ;; Idris

    (idris-repl-input-face
     ((t :inherit default :foreground ,mid-grey :weight normal)))

    (idris-semantic-data-face
     ((t :inherit default)))

    (idris-semantic-type-face
     ((t :inherit default)))

    (idris-semantic-bound-face
     ((t :inherit default :slant italic)))

    (idris-semantic-implicit-face
     ((t :inherit default)))

    (idris-inline-doc-face
     ((t :inherit font-lock-comment-face)))

    ;; FStar

    (fstar-subp-overlay-processed-face
     ((t :background ,subtle-bg)))

    (fstar-subp-overlay-busy-face
     ((t :background ,pending-bg)))

    (fstar-structure-face
     ((t :inherit font-lock-keyword-face)))

    (fstar-subscript-face
     ((t :height 0.65)))

    (fstar-literate-comment-face
     ((t :inherit font-lock-comment-face)))

    ;; web-mode

    (web-mode-function-call-face
     ((t :weight normal)))

    (web-mode-json-key-face
     ((t :weight normal)))

    ;; Pairs

    (show-paren-match
     ((t
       :weight bold
       :foreground ,paren-match
       :underline ,paren-match)))

    (show-paren-mismatch
     ((t
       :weight bold
       :foreground ,error-fg
       :underline ,error-fg)))

    ;; Ivy

    (minibuffer-prompt
     ((t :foreground ,default-fg)))

    (ivy-current-match
     ((t :foreground ,emphasis-1 :background ,highlight-bg)))

    (ivy-minibuffer-match-face-1
     ((t :background ,subtle-bg)))

    (ivy-minibuffer-match-face-2
     ((t :background ,subtle-bg)))

    ;; Highlights

    (highlight
     ((t :background ,highlight-bg)))

    (iedit-occurrence
     ((((background light))
       :weight normal
       :background "#FFe0e0"
       :foreground ,default-fg)
      (((background dark))
       :weight normal
       :background "#703030"
       :foreground ,default-fg)))

    (evil-search-highlight-persist-highlight-face
     ((t :inherit highlight :background nil)))

    (highlight-thing
     ((t :weight demibold :foreground ,emphasis-1)))

    (hexl-ascii-region
     ((t (:inherit highlight-thing))))

    (hexl-address-region
     ((t (:inherit highlight-thing :underline t))))


    (ahs-face
     ((t :inherit highlight)))

    (ahs-plugin-whole-buffer-face
     ((t :inherit highlight)))


    ;; Ediff

    (ediff-odd-diff-A
     ((t :inherit highlight)))
    (ediff-odd-diff-B
     ((t :inherit highlight)))
    (ediff-odd-diff-C
     ((t :inherit highlight)))

    (ediff-current-diff-C
     ((((background light)) :background "#ffffaa")
      (((background dark)) :background "#444477")))
    (ediff-fine-diff-C
     ((((background light)) :background "#ffff55")
      (((background dark)) :background "#666699")))


    ;; Magit

    (magit-signature-untrusted
     ((t :foreground ,cb-theme-common-cyan)))

    (magit-diff-file-heading
     ((t :weight normal)))

    (magit-section-heading
     ((t :weight demibold)))

    (magit-section-highlight
     ((t :background ,subtle-bg)))

    (magit-diff-context-highlight
     ((((background light))
       :background "grey90"
       :foreground "grey50")
      (((background dark))
       :background "grey20"
       :foreground "grey70")))

    (magit-popup-disabled-argument
     ((t :foreground ,mid-grey)))

    (magit-popup-option-value
     ((t :weight normal)))

    (magithub-ci-success
     ((t :inherit success)))

    ;; Outline, Org

    (org-link
     ((t :inherit link :weight normal)))

    (org-block
     ((t :background ,subtle-bg)))

    (org-done
     ((t
       :inherit default
       :weight bold
       :foreground ,dimmed-fg)))

    (org-document-info
     ((t :foreground ,default-fg :weight demibold)))

    (org-document-info-keyword
     ((t :weight light)))

    (org-scheduled-today
     ((t :inherit default)))

    (org-date
     ((t :weight light)))

    (org-sexp-date
     ((t :weight light)))

    (org-date
     ((t :underline t)))

    (org-agenda-date-today
     ((t :foreground ,emphasis-1 :weight demibold)))

    (org-agenda-date-weekend
     ((t :inherit org-agenda-date)))

    (org-warning
     ((t :inherit warning)))

    (org-upcoming-deadline
     ((t :foreground ,emphasis-1 :weight normal)))

    (org-scheduled-previously
     ((t :foreground ,error-fg)))

    (org-formula
     ((t :weight light)))

    (org-tag
     ((t :weight light)))

    (org-table
     ((t :inherit default)))

    (org-scheduled
     ((t :inherit default)))

    (org-meta-line
     ((t :foreground ,dimmed-fg :weight light)))

    (org-agenda-structure
     ((t :weight demibold)))

    (org-document-title
     ((t :weight bold)))

    (outline-1
     ((t :weight demibold)))
    (outline-2
     ((t :inherit outline-1)))
    (outline-3
     ((t :inherit outline-1)))
    (outline-4
     ((t :inherit outline-1)))
    (outline-5
     ((t :inherit outline-1)))
    (outline-6
     ((t :inherit outline-1)))
    (outline-7
     ((t :inherit outline-1)))
    (outline-8
     ((t :inherit outline-1)))
    (outline-9
     ((t :inherit outline-1)))

    ;; Ledger

    (ledger-font-comment-face
     ((t :weight light)))

    (ledger-font-posting-date-face
     ((t :inherit default)))

    (ledger-font-posting-account-face
     ((t :inherit default)))

    (ledger-font-xact-highlight-face
     ((t nil)))

    (ledger-font-other-face
     ((t :inherit default :weight demibold)))

    (ledger-font-directive-face
     ((t :inherit default)))

    (ledger-font-posting-amount-face
     ((t :inherit default)))

    ;; Mu4e

    (mu4e-title-face
     ((t :weight demibold)))

    (mu4e-header-key-face
     ((t :weight demibold)))

    (mu4e-highlight-face
     ((t :foreground ,emphasis-1 :weight demibold)))

    (mu4e-header-highlight-face
     ((t :inherit highlight)))

    ;; Message composition

    (message-header-name
     ((t :weight demibold)))

    (message-header-to
     ((t :weight normal)))

    (message-header-cc
     ((t :weight light :foreground ,mid-grey)))

    (message-cited-text
     ((t :weight light :foreground ,mid-grey)))

    (message-header-subject
     ((t :weight normal)))

    (message-header-other
     ((t :weight demibold)))

    ;; Neotree

    (neo-root-dir-face
     ((t :foreground ,emphasis-1)))

    (neo-banner-face
     ((t :foreground ,emphasis-1 :weight light)))

    (neo-file-link-face
     ((t :weight light)))

    (neo-dir-link-face
     ((t :weight normal)))

    ;; Cargo & Rust

    (cargo-process--ok-face
     ((t :inherit success)))

    (cargo-process--warning-face
     ((t :inherit warning)))

    (cargo-process--error-face
     ((t :inherit error)))

    (cargo-process--standard-face
     ((t :weight demibold)))

    (cargo-process--standard-face
     ((t :weight demibold)))

    (cb-rust-faces-macro
     ((t :weight normal)))

    (cb-rust-faces-bool
     ((t :weight normal)))

    ;; Term

    (term-color-black
     ((t :foreground ,default-fg)))

    (term-color-blue
     ((t :foreground ,emphasis-1)))

    (term-color-bold
     ((t :weight demibold)))

    (term-color-cyan
     ((t :foreground ,cb-theme-common-cyan)))

    (term-color-green
     ((t :foreground ,cb-theme-common-green)))

    (term-color-magenta
     ((t :foreground ,cb-theme-common-magenta)))

    (term-color-red
     ((t :foreground ,cb-theme-common-red)))

    (term-color-underline
     ((t :underline t)))

    (term-color-white
     ((t :foreground ,default-fg)))

    (term-color-yellow
     ((t :foreground ,cb-theme-common-yellow)))

    ;; Markdown

    (markdown-line-break-face
     ((t :underline ,emphasis-1)))

    ;; Kubernetes

    (kubernetes-dimmed
     ((t :foreground ,dimmed-fg)))

    ;; Scala

    (scala-font-lock:var-keyword-face
     ((t :inherit error)))

    (scala-font-lock:var-face
     ((t :inherit font-lock-variable-name-face)))

    ;; Git time machine

    (git-timemachine-minibuffer-detail-face
     ((t :foreground ,emphasis-1)))

    (git-timemachine-minibuffer-author-face
     ((t :inherit default)))

    ;; Company

    (tooltip
     ((default :foreground "black")
      (((class color) (min-colors 88) (background light))
       (:background "cornsilk"))
      (((class color) (min-colors 88) (background dark))
       (:background "yellow"))))

    (company-tooltip
     ((t :inherit tooltip)))

    (company-template-field
     ((t :inherit highlight)))

    (pos-tip-temp
     ((t :inherit tooltip)))

    ;; Latex

    (font-latex-bold-face
     ((t :weight demibold)))

    (font-latex-warning-face
     ((t :inherit warning)))

    (font-latex-sectioning-5-face
     ((t :foreground ,emphasis-1 :weight demibold)))

    (font-latex-sedate-face
     ((t :weight light :foreground ,dimmed-fg)))

    ;; Misc faces

    (page-break-lines
     ((t :weight light :foreground ,dimmed-fg)))

    (go-peg-mode-production-name
     ((t :weight demibold)))

    (sh-quoted-exec
     ((t :weight demibold)))

    (sh-heredoc
     ((t :inherit font-lock-string-face)))

    (hl-todo
     ((t :foreground nil :box t)))

    (parenthesis
     ((t :weight light)))

    (link
     ((t :inherit default :underline t)))))

(provide 'cb-theme-common)

;;; cb-theme-common.el ends here
