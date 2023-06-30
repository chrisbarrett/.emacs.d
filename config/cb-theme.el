;;; cb-theme.el --- Colour theme  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'ht)

(use-package doom-themes
  :demand t
  :autoload (doom-blend)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(defconst cb-theme-light 'doom-solarized)
(defconst cb-theme-dark 'doom-one)



(defun cb-append-faces (&rest specs)
  `((t ,@(ht-to-plist (seq-reduce (pcase-lambda (acc `((,_pred . ,attrs)))
                                    (ht-merge acc (ht-from-plist attrs)))
                                  specs
                                  (ht-create))))))

(defconst cb-theme-settings
  (let* ((dark-fg "#bbc2cf")
         (light-fg "#556b72")
         (dark-bg "#282c34")
         (light-bg "#FDF6E3")
         (title `((t :height 1.5 :weight normal)))
         (heading `((t :height 1.2)))
         (block-markup `((t :inherit font-lock-comment-face :background unspecified :italic nil :weight light :bold nil)))
         (outline-heading `((t :weight semibold :foreground unspecified)))
         (bullet '((((background light))
                    (:foreground "#96A7A9"))
                   (((background dark))
                    (:foreground "#5B6268")))))
    `((font-lock-comment-face
       ((((background dark))
         (:foreground "#869799" :italic t))))
      (font-lock-doc-face
       ((((background dark))
         (:foreground "#869799" :italic t))))
      (font-lock-constant-face
       ((((background dark))
         (:foreground "#a9a1e1" :weight semibold))
        (((background light))
         (:foreground "#6c71c4" :weight semibold))))
      (parenthesis
       ((((background dark))
         (:foreground "#869799" :italic nil :weight light :inherit unspecified))
        (((background light))
         (:foreground "#9ca0a4" :italic nil :weight light :inherit unspecified))))
      (org-transclusion-fringe
       ((((background dark))
         (:foreground ,(doom-blend dark-bg "#51afef" 0.5) :weight bold))
        (((background light))
         (:foreground ,(doom-blend light-bg "#268bd2" 0.4) :weight bold))))
      (org-roam-search-highlight
       ((((background dark))
         (:foreground "#98be65" :background "#3e493d" :weight bold :inherit magit-diff-added-highlight))
        (((background light))
         (:foreground "#859900" :background "#e5e3b5" :weight bold :inherit magit-diff-added-highlight))))
      (dirvish-hl-line
       ((((background dark))
         (:background ,(doom-blend dark-bg "#51afef" 0.5) :extend t))
        (((background light))
         (:background ,(doom-blend light-bg "#268bd2" 0.4) :extend t))))
      (org-superstar-header-bullet ,bullet)
      (org-agenda-clocking
       ((((background dark))
         (:foreground ,dark-fg :extend t))
        (((background light))
         (:foreground ,light-fg :extend t))))
      (magit-section-secondary-heading
       ((((background dark))
         (:foreground "#a9a1e1" :weight semibold :extend t))
        (((background light))
         (:foreground "#6c71c4" :weight semibold :extend t))))
      (org-document-info-keyword ((t :italic nil)) t)
      (org-meta-line ((t :inherit font-lock-comment-face :italic nil :foreground unspecified)) t)
      (org-link ((t :weight unspecified :underline nil)) t)
      (org-footnote ((t :foreground unspecified :slant italic :inherit font-lock-comment-face)))
      (compilation-warning ((t :italic nil)))
      (magit-header-line ((t :background unspecified :box nil)))
      (org-document-title ,title)
      (org-roam-header-line ((t :inherit org-document-title)))
      (vertico-group-title ((t :inherit magit-section-heading)))
      (consult-line-number ((t :inherit font-lock-comment-face)))
      (shortdoc-section ((t :inherit fixed-pitch)))
      (org-roam-review-tags-filter ((t :inherit magit-section-heading :bold nil)))
      (org-roam-review-heading ,outline-heading)

      (edebug-enabled-breakpoint ((t :bold t :inherit error)))
      (edebug-disabled-breakpoint ((t :bold t :inherit font-lock-builtin-face)))

      (org-roam-custom-node-topic ((t :height 1.5 :slant italic :weight normal :inherit font-lock-constant-face)))

      (outline-1 ,outline-heading)
      (outline-2 ,outline-heading)
      (outline-3 ,outline-heading)
      (outline-4 ,outline-heading)
      (outline-5 ,outline-heading)
      (outline-6 ,outline-heading)
      (outline-7 ,outline-heading)
      (outline-8 ,outline-heading)
      (org-roam-title ((t :inherit magit-section-secondary-heading)))
      (org-roam-olp ((t :inherit magit-section-secondary-heading :italic t :bold nil)))
      (org-agenda-structure ,(cb-append-faces outline-heading heading))
      (link ((t :weight unspecified)) t)
      (highlight ((t :inherit unspecified :foreground unspecified :background unspecified :bold t)))
      (highlight-thing ((t (:inherit highlight))))
      (org-drawer ,block-markup)
      (org-list-dt ((t :italic t :foreground unspecified)))
      (org-agenda-done ((t :inherit org-done :bold nil)) t)
      (org-todo ((t :weight light)))
      (org-done ((t :weight light)))
      (mode-line ((t :inherit default)))
      (font-lock-builtin-face ((t :italic nil)) t)
      (font-lock-keyword-face ((t :weight normal :bold nil)) t)
      (org-transclusion-keyword ((t :inherit org-meta-line)))
      (org-block-begin-line ,block-markup)
      (org-block ((t :background unspecified)))
      (org-block-end-line ,block-markup)

      (markdown-header-delimiter-face ,bullet)
      (markdown-table-face ((t :inherit org-table)))
      (markdown-header-face-1 ,title)
      (markdown-header-face-2 ,(cb-append-faces outline-heading heading))
      (markdown-header-face-3 ,(cb-append-faces outline-heading '((t :italic t))))
      (markdown-header-face-4 ,(cb-append-faces outline-heading '((t :underline t)))))))

(defun cb-theme-apply-settings ()
  (apply 'custom-theme-set-faces 'user cb-theme-settings))



(defun cb-themeing-gtk ()
  "Select a theme setting depending on the current OS theme."
  (with-temp-buffer
    (ignore-errors
      (call-process "gsettings" nil t nil
                    "get" "org.gnome.desktop.interface" "gtk-theme"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(defun cb-themeing-macos ()
  (with-temp-buffer
    (ignore-errors
      (call-process "defaults" nil t nil
                    "read" "-g" "AppleInterfaceStyle"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(cl-defun cb-theme-for-system-type (&key light dark)
  (let ((theme
         (pcase system-type
           ('gnu/linux (cb-themeing-gtk))
           ('darwin (cb-themeing-macos)))))
    (if (equal 'dark theme)
        dark
      light)))



;;; Define Lisp functions for switching theme via emacsclient

;; I have `dark' and `light' scripts I execute to change theme across all my
;; applications. The following functions will be invoked by those scripts over
;; =emacsclient=.

(defun cb-theme-light ()
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-solarized-light t))

(defun cb-theme-dark ()
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-one t))

(provide 'cb-theme)

;;; cb-theme.el ends here
