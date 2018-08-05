;;; cb-theme-common.el --- Common configuration for light and dark themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'dash)

(defconst cb-theme-common-font-family
  (--first (find-font (font-spec :name it))
           '("Iosevka"
             "Hasklig"
             "Source Code Pro"
             "Source Code Variable"
             "DejaVu Sans Mono"
             "Menlo")))

;; TODO: Maybe move this into a host-specific way of setting config.
(defconst cb-theme-common-default-height
  (pcase system-type
    (`darwin
     160)
    (_
     130)))

(cl-defun cb-theme-common-make-theme (&key
                       default-fg
                       default-bg
                       region-bg
                       mode-line-bg
                       subtle-bg
                       highlight-bg
                       paren-match
                       emphasis

                       (blue "#268bd2")
                       (cyan "#2aa198")
                       (dark-grey "grey40")

                       (green "#859900")
                       (light-grey "grey80")
                       (magenta "#d33682")
                       (mid-grey "grey60")
                       (orange "#cb4b16")
                       (pending-bg "#AAAA33")
                       (pink "pink")
                       (red "red1")
                       (yellow "#b58900")

                       (dimmed-fg mid-grey)
                       (light-weight 'extra-light)
                       (mode-line-fg default-bg)
                       (error-fg red)

                       (heavy-bg mode-line-fg)
                       (heavy-fg default-bg))
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
     ((t :weight normal :foreground ,green)))
    (warning
     ((t :weight normal :foreground ,orange)))
    (error
     ((t :weight normal :foreground ,error-fg)))

    (link
     ((((background light))
       :weight ,light-weight :underline ,light-grey)
      (((background dark))
       :weight ,light-weight :underline ,dark-grey)))

    (fringe
     ((t :background ,default-bg)))

    (vertical-border
     ((t
       :background ,mode-line-bg
       :foreground ,mode-line-bg)))

    (mode-line
     ((t
       :background ,mode-line-bg
       :foreground ,mode-line-fg :weight demibold
       :box (:line-width 3 :color ,mode-line-bg))))

    (mode-line-format-nonemphasised-element
     ((t :weight ,light-weight)))

    (mode-line-format-emphasised-element
     ((t :weight ,light-weight :foreground ,emphasis)))

    (evil-transient-state-title-face
     ((t :inherit mode-line :background ,mode-line-bg)))

    (region
     ((t :background ,region-bg)))

    (hl-line
     ((t :background ,subtle-bg)))

    ;; General font-lock faces.

    (font-lock-keyword-face
     ((t :weight ,light-weight)))

    (font-lock-builtin-face
     ((t :weight ,light-weight)))

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
     ((t :weight ,light-weight)))

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
     ((t :foreground ,emphasis)))

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
     ((t :weight ,light-weight)))

    (diredp-date-time
     ((t :foreground ,mid-grey :weight ,light-weight)))

    (diredp-dir-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (diredp-no-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (diredp-rare-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (diredp-exec-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (diredp-read-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (diredp-write-priv
     ((t :weight ,light-weight :foreground ,mid-grey)))

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

    (web-mode-json-context-face
     ((t :inherit default)))

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
     ((t :foreground ,emphasis :background ,region-bg)))

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
     ((t :weight demibold :foreground ,emphasis)))

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
    (ediff-even-diff-A
     ((t :inherit highlight)))
    (ediff-even-diff-B
     ((t :inherit highlight)))
    (ediff-even-diff-C
     ((t :inherit highlight)))

    (ediff-current-diff-C
     ((((background light)) :background "#ffffaa")
      (((background dark)) :background "#444477")))
    (ediff-fine-diff-C
     ((((background light)) :background "#ffff55")
      (((background dark)) :background "#666699")))


    ;; Magit

    (magit-signature-untrusted
     ((t :foreground ,cyan)))

    (magit-blame-heading
     ((((background light))
       :background "#cceecc"
       :foreground "#22aa22"
       :weight ,light-weight)
      (((background dark))
       :background "#1a331a"
       :foreground "#cceecc"
       :weight ,light-weight)))

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

    ;; compilation

    (cb-compilation-base-face
     ((t :foreground ,dimmed-fg)))

    ;; rg

    (rg-file-tag-face
     ((t :foreground ,dimmed-fg)))

    (rg-filename-face
     ((t :foreground ,emphasis)))

    (rg-match-face
     ((t :inherit highlight)))

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
     ((t :foreground ,dimmed-fg :weight ,light-weight)))

    (org-scheduled-today
     ((t :inherit default)))

    (org-date
     ((t :weight ,light-weight)))

    (org-sexp-date
     ((t :weight ,light-weight)))

    (org-date
     ((t :underline t)))

    (org-agenda-date-today
     ((t :foreground ,emphasis :weight demibold)))

    (org-agenda-date-weekend
     ((t :inherit org-agenda-date)))

    (org-warning
     ((t :inherit warning)))

    (org-upcoming-deadline
     ((t :foreground ,emphasis :weight normal)))

    (org-scheduled-previously
     ((t :foreground ,error-fg)))

    (org-formula
     ((t :weight ,light-weight)))

    (org-tag
     ((t :weight ,light-weight)))

    (org-table
     ((t :inherit default)))

    (org-scheduled
     ((t :inherit default)))

    (org-meta-line
     ((t :foreground ,dimmed-fg :weight ,light-weight)))

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
     ((t :weight ,light-weight)))

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
     ((t :foreground ,emphasis :weight demibold)))

    (mu4e-header-highlight-face
     ((t :inherit highlight)))

    ;; Message composition

    (message-header-name
     ((t :weight demibold)))

    (message-header-to
     ((t :weight normal)))

    (message-header-cc
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (message-cited-text
     ((t :weight ,light-weight :foreground ,mid-grey)))

    (message-header-subject
     ((t :weight normal)))

    (message-header-other
     ((t :weight demibold)))

    ;; Treemacs

    (treemacs-root-face
     ((t :foreground ,emphasis)))

    (treemacs-directory-face
     ((t :weight normal)))

    (treemacs-git-modified-face
     ((t :slant italic)))

    (treemacs-git-added-face
     ((t :underline ,blue)))

    (treemacs-git-untracked-face
     ((t :underline t)))

    (treemacs-git-ignored-face
     ((t :foreground ,dimmed-fg)))

    ;; Neotree

    (neo-root-dir-face
     ((t :foreground ,emphasis)))

    (neo-banner-face
     ((t :foreground ,emphasis :weight ,light-weight)))

    (neo-file-link-face
     ((t :weight ,light-weight)))

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
     ((t :foreground ,emphasis)))

    (term-color-bold
     ((t :weight demibold)))

    (term-color-cyan
     ((t :foreground ,cyan)))

    (term-color-green
     ((t :foreground ,green)))

    (term-color-magenta
     ((t :foreground ,magenta)))

    (term-color-red
     ((t :foreground ,red)))

    (term-color-underline
     ((t :underline t)))

    (term-color-white
     ((t :foreground ,default-fg)))

    (term-color-yellow
     ((t :foreground ,yellow)))

    ;; Markdown

    (markdown-line-break-face
     ((t :underline ,emphasis)))

    ;; Imenu-List

    (imenu-list-entry-face-0
     ((t :foreground nil)))

    (imenu-list-entry-face-1
     ((t :foreground nil)))


    (imenu-list-entry-subalist-face-0
     ((t :inherit imenu-list-entry-face-0 :weight ,light-weight)))

    (imenu-list-entry-subalist-face-1
     ((t :inherit imenu-list-entry-face-1 :weight ,light-weight)))

    (imenu-list-entry-subalist-face-2
     ((t :inherit imenu-list-entry-face-2 :weight ,light-weight)))

    (imenu-list-entry-subalist-face-3
     ((t :inherit imenu-list-entry-face-3 :weight ,light-weight)))

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
     ((t :foreground ,emphasis)))

    (git-timemachine-minibuffer-author-face
     ((t :inherit default)))

    ;; Company

    (tooltip
     ((default :foreground "black")
      (((class color) (min-colors 88) (background light))
       (:background "cornsilk"))
      (((class color) (min-colors 88) (background dark))
       (:background "yellow"))))

    (company-box-annotation
     ((t :foreground ,dimmed-fg)))

    (company-box-candidate
     ((t :foreground ,heavy-fg)))

    (company-box-background
     ((t :background ,heavy-bg)))

    (company-box-selection
     ((t :foreground ,emphasis :background ,region-bg)))

    (company-tooltip
     ((t :background ,subtle-bg)))

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
     ((t :foreground ,emphasis :weight demibold)))

    (font-latex-sedate-face
     ((t :weight ,light-weight :foreground ,dimmed-fg)))

    ;; Hydra

    (hydra-face-amaranth
     ((t :weight demibold :foreground ,red)))

    (hydra-face-pink
     ((t :weight demibold :foreground ,red)))

    (hydra-face-blue
     ((t :weight demibold :foreground ,blue)))

    (hydra-face-red
     ((t :weight demibold :foreground ,red)))

    (hydra-face-teal
     ((t :weight demibold :foreground ,blue)))

    ;; Misc faces

    (page-break-lines
     ((t :weight ,light-weight :foreground ,dimmed-fg)))

    (go-peg-mode-production-name
     ((t :weight demibold)))

    (sh-quoted-exec
     ((t :weight demibold)))

    (sh-heredoc
     ((t :inherit font-lock-string-face)))

    (hl-todo
     ((t :foreground nil :box t)))

    (parenthesis
     ((t :weight ,light-weight)))

    (link
     ((t :inherit default :underline t)))))

(provide 'cb-theme-common)

;;; cb-theme-common.el ends here
