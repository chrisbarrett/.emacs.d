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

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package delight
  :demand t)

(setq-default cursor-in-non-selected-windows nil)



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



(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode)
  :custom
  (page-break-lines-modes '(prog-mode org-agenda-mode latex-mode help-mode)))

(use-package paren-face
  :hook (after-init . global-paren-face-mode)
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

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . enable-hl-todo-unless-org-buffer))
  :preface
  (defun enable-hl-todo-unless-org-buffer ()
    (unless (derived-mode-p 'org-mode)
      (hl-todo-mode)))
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

(use-package default-text-scale
  :custom
  (default-text-scale-amount 30))

(use-package ligature
  :hook (after-init . global-ligature-mode)
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

(provide 'cb-theme)

;;; cb-theme.el ends here
