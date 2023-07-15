;;; cb-theme.el --- Colour theme  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cl-lib)
(require 'ht)

(use-package doom-themes :ensure t :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(use-package hl-line :hook (after-init . global-hl-line-mode))

(use-package delight :ensure t :demand t)

(setq-default cursor-in-non-selected-windows nil)



(defun cb-append-faces (&rest specs)
  "Smash SPECS together."
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
  "Select a theme setting depending on the current OS theme."
  (with-temp-buffer
    (ignore-errors
      (call-process "defaults" nil t nil
                    "read" "-g" "AppleInterfaceStyle"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(cl-defun cb-theme-for-system-type (&key light dark)
  "Return either LIGHT or DARK value depending on current OS theme."
  (let ((theme (pcase system-type
                 ('gnu/linux
                  (cb-themeing-gtk))
                 ('darwin
                  (cb-themeing-macos)))))
    (if (equal 'dark theme)
        dark
      light)))

;; Set reasonable placeholder foreground and background colours until the main
;; theme is loaded, according to the WM theme.

(set-background-color (cb-theme-for-system-type :dark "#282c34" :light "#FDF6E3"))
(set-foreground-color (cb-theme-for-system-type :dark "#bbc2cf" :light "#556b72"))



;;; Header line & mode line

(defvar cb-theme--selected-window-for-mode-line-format nil)

(define-advice pre-redisplay-function (:before (_windows) update-selected-window-for-mode-line-format)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq cb-theme--selected-window-for-mode-line-format (selected-window))))

(defconst cb-theme--mode-line-selected-window-indicator '(:eval (if (equal (get-buffer-window) cb-theme--selected-window-for-mode-line-format)
                                                                    "● "
                                                                  "   ")))

(defconst cb-theme-mode-or-header-line-format
  (let ((env-info 'mode-line-modified)
        (buffer-info '(mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position))
        (additional-info '(mode-line-modes mode-line-misc-info)))
    `("%e" mode-line-front-space cb-theme--mode-line-selected-window-indicator ,env-info ,@buffer-info (vc-mode vc-mode) "  " ,@additional-info mode-line-end-spaces)))

(setq-default header-line-format cb-theme-mode-or-header-line-format)
(setq-default mode-line-format nil)

(use-package minions :ensure t :demand t
  :custom
  (minions-mode-line-lighter "...")
  (minions-direct '(auto-revert-mode git-auto-commit-mode flymake-mode))
  :preface
  (define-minor-mode minions-header-line-mode
    "Change the display of minor modes in the header line."
    :group 'cb-theme
    :global t
    (if minions-header-line-mode
        (setq-default header-line-format
                      (cl-subst 'minions-mode-line-modes
                                'mode-line-modes
                                (default-value 'header-line-format)
                                :test #'equal))
      (cl-nsubst 'mode-line-modes 'minions-mode-line-modes header-line-format)))
  :config
  (minions-header-line-mode +1))



;; Disable cursor blinking.
(blink-cursor-mode -1)

;;; Run a hook when the theme changes

(defvar after-load-theme-functions nil)

(define-advice load-theme (:after (theme &rest _) run-hook)
  (run-hook-with-args 'after-load-theme-functions theme))



;;; Define Lisp functions for switching theme via emacsclient

;; I have `dark' and `light' scripts I execute to change theme across all my
;; applications. The following functions will be invoked by those scripts over
;; =emacsclient=.

(defconst cb-theme-light 'doom-solarized-light)
(defconst cb-theme-dark 'doom-one)

(defun cb-theme-light ()
  "Enable light colour theme."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme cb-theme-light t))

(defun cb-theme-dark ()
  "Enable dark colour theme."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme cb-theme-dark t))

;; Finally, load the appropriate colour theme.

(with-demoted-errors "cb-theme: %s"
  (cb-theme-apply-settings)
  (load-theme (cb-theme-for-system-type :light cb-theme-light :dark cb-theme-dark) t))

;; KLUDGE: Something weird is clobbering settings in org-mode. Reapply the user
;; theme when starting up org-mode.
(add-hook 'org-mode-hook #'cb-theme-apply-settings)



(use-package page-break-lines :ensure t :hook (after-init . global-page-break-lines-mode)
  :custom
  (page-break-lines-modes '(prog-mode org-agenda-mode latex-mode help-mode)))

(use-package paren-face :ensure t :hook (after-init . global-paren-face-mode)
  :defines (paren-face-modes)
  :custom
  (paren-face-regexp (rx (any "{}();,")))
  :config
  (add-to-list 'paren-face-modes 'csharp-mode)
  (add-to-list 'paren-face-modes 'js-base-mode)
  (add-to-list 'paren-face-modes 'lisp-data-mode)
  (add-to-list 'paren-face-modes 'typescript-base-mode)
  (add-to-list 'paren-face-modes 'yaml-ts-mode)
  (font-lock-add-keywords 'js-base-mode `((,(rx (any ":")) 0 'parenthesis)))
  (font-lock-add-keywords 'typescript-base-mode `((,(rx (any ":")) 0 'parenthesis))))

(use-package hl-todo :ensure t :hook (after-init . global-hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   (seq-map (lambda (it) (cons it 'hl-todo))
            '("TODO"
              "NEXT"
              "HACK"
              "FIXME"
              "KLUDGE"
              "PATCH"
              "NOTE"))))

(use-package default-text-scale :ensure t
  :custom
  (default-text-scale-amount 30))

(use-package highlight-thing :ensure t :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-delay-seconds 0.1)
  (highlight-thing-limit-to-defun nil)
  (highlight-thing-case-sensitive-p t)

  ;; Suppress highlight when hovering over certain kinds of symbols
  :preface
  (defun cb-face-ancestors (face)
    "List all faces that FACE transitively inherits from."
    (let (result)
      (while (and face (not (equal face 'unspecified)))
        (setq result (cons face result))
        (setq face (face-attribute face :inherit)))
      (nreverse result)))

  (define-advice highlight-thing-should-highlight-p (:filter-return (res) highlight-only-some)
    (when res
      (let ((excluded-faces '(font-lock-string-face
                              font-lock-keyword-face
                              font-lock-comment-face
                              font-lock-preprocessor-face
                              font-lock-builtin-face))
            (faces (seq-mapcat #'cb-face-ancestors (face-at-point nil t))))
        (null (seq-intersection faces excluded-faces))))))

;; Load vhl at compile-time so macro expansions are available
(cl-eval-when (compile)
  (require 'volatile-highlights))

(use-package volatile-highlights :ensure t :demand t
  :config
  (volatile-highlights-mode))

(use-package ligature :ensure t :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'org-mode '(
                                      ;; ;; ;;;
                                      (";" (rx (+ ";")))
                                      ;; && &&&
                                      ("&" (rx (+ "&")))
                                      ;; !! !!! !. !: !!.
                                      ("!" (rx (+ (or "!" "\." ":"))))
                                      ;; ?? ??? ?:   ?.
                                      ("?" (rx (or ":" "\." (+ "?"))))
                                      ;; %% %%%
                                      ("%" (rx (+ "%")))
                                      ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                                      ;; |->>-||-<<-| |- |== ||=||
                                      ;; |==>>==<<==<=>==//==/=!==:===>
                                      ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                                      "-" "=" ))))
                                      ;; \\ \\\ \/
                                      ("\\" (rx (or "/" (+ "\\"))))
                                      ;; :: ::: :::: :> :< ://
                                      (":" (rx (or ">" "<" "//" (+ ":"))))
                                      ;; // /// //// /\ /* />
                                      ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"))))
                                      ;; .. ... .... .- .? ..<
                                      ("\." (rx (or "-" "\?" "\.<" (+ "\."))))
                                      ;; -- --- ---- -> ->> -| -|->-->>->--<<-|
                                      ("-" (rx (+ (or ">" "<" "|" "-"))))
                                      ;; *> */ *)  ** *** ****
                                      ("*" (rx (or ">" "/" ")" (+ "*"))))
                                      ;; www wwww
                                      ("w" (rx (+ "w")))
                                      ;; <> <!-- <|> <: <~ <~> <~~ <$ </  <+> <*>
                                      ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                                      ;; <<->
                                      ;; << <<< <<<<
                                      ("<" (rx (+ (or "\$" "<" ">" ":" "!" "-"  "/" "|"))))
                                      ;; >: >- >>- >--|-> >>-|-> >> >>> >>>>
                                      (">" (rx (+ (or ">" "<" "|" "/" ":" "-"))))
                                      ;; #: #! #( #? #[ #{ ## ### #####
                                      ("#" (rx (or ":" "!" "(" "\?" "\[" "{" (+ "#"))))
                                      ;; Fira code: 0xFF 0x12
                                      ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                                      ;; Fira code:
                                      "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                                      ;; The few not covered by the regexps.
                                      "{|"  "[|"  "]#"  "(*"  "}#"  "$>"
                                      ))
  (dolist (mode '(prog-mode markdown-mode nxml-mode))
    (ligature-set-ligatures mode
                            '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                              ;; =:= =!=
                              ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                              ;; ;; ;;;
                              (";" (rx (+ ";")))
                              ;; && &&&
                              ("&" (rx (+ "&")))
                              ;; !! !!! !. !: !!. != !== !~
                              ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                              ;; ?? ??? ?:  ?=  ?.
                              ("?" (rx (or ":" "=" "\." (+ "?"))))
                              ;; %% %%%
                              ("%" (rx (+ "%")))
                              ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                              ;; |->>-||-<<-| |- |== ||=||
                              ;; |==>>==<<==<=>==//==/=!==:===>
                              ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                              "-" "=" ))))
                              ;; \\ \\\ \/
                              ("\\" (rx (or "/" (+ "\\"))))
                              ;; ++ +++ ++++ +>
                              ("+" (rx (or ">" (+ "+"))))
                              ;; :: ::: :::: :> :< := :// ::=
                              (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                              ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                              ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                              "="))))
                              ;; .. ... .... .= .- .? ..= ..<
                              ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                              ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                              ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                              ;; *> */ *)  ** *** ****
                              ("*" (rx (or ">" "/" ")" (+ "*"))))
                              ;; www wwww
                              ("w" (rx (+ "w")))
                              ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                              ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                              ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                              ;; << <<< <<<<
                              ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                              "-"  "/" "|" "="))))
                              ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                              ;; >> >>> >>>>
                              (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                              ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                              ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                           (+ "#"))))
                              ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                              ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                              ;; __ ___ ____ _|_ __|____|_
                              ("_" (rx (+ (or "_" "|"))))
                              ;; Fira code: 0xFF 0x12
                              ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                              ;; Fira code:
                              "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                              ;; The few not covered by the regexps.
                              "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))))

(use-package prog-mode
  :hook
  ((lisp-data-mode scheme-mode) . cb-prettify-symbols-lisp-setup)
  :preface
  (defun cb-prettify-symbols-lisp-setup ()
    (setq-local prettify-symbols-alist '(("lambda" . #x3bb)))
    (prettify-symbols-mode +1))
  :config
  (advice-add 'prettify-symbols--post-command-hook :around 'advice-ignore-errors))

(provide 'cb-theme)

;;; cb-theme.el ends here
