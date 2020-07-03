;;; config-editing.el --- Config for simple editing packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)
(require 'f)
(require 'major-mode-hydra)

;; Auto-indent on RET

(general-define-key :keymaps '(text-mode-map prog-mode-map)
  "RET" #'comment-indent-new-line)


;; Enable hideshow in all programming buffers.

(autoload 'hs-minor-mode "hideshow")
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; Define line transposition commands.

(autoload 'org-move-item-down "org-list")
(autoload 'org-move-item-up "org-list")

(defun config-editing-transpose-line-up ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-up)

    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode)))

(defun config-editing-transpose-line-down ()
  "Move the current line up."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-move-item-down)

    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)))

(global-set-key (kbd "C-<up>") #'config-editing-transpose-line-up)
(global-set-key (kbd "C-<down>") #'config-editing-transpose-line-down)

(global-set-key (kbd "s-<up>") #'config-editing-transpose-line-up)
(global-set-key (kbd "s-<down>") #'config-editing-transpose-line-down)



;; Define some editing commands.

;; Define a command to indent every line in the buffer. This should really be a
;; thing out-of-the-box.

(defun config-editing-indent-buffer ()
  "Indent the entire buffer."
  (interactive "*")
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(unless (fboundp 'indent-buffer)
  (defalias 'indent-buffer #'config-editing-indent-buffer))

(defun config-indent-dwim (&optional justify)
  "Indent the thing at point.

Knows how to fill strings and comments, or indent code.

Optional arg JUSTIFY will justify comments and strings."
  (interactive "*P")
  (-let [(_ _ _ string-p comment-p) (syntax-ppss)]
    (cond
     (string-p
      (let ((progress (make-progress-reporter "Filling paragraph")))
        (fill-paragraph justify)
        (progress-reporter-done progress)))
     (comment-p
      (let ((progress (make-progress-reporter "Filling comment")))
        (fill-comment-paragraph justify)
        (progress-reporter-done progress)))

     ((region-active-p)
      (if (bound-and-true-p lsp-mode)
          (lsp-format-region (region-beginning) (region-end))
        (indent-region (region-beginning) (region-end))))
     (t
      (let ((progress (make-progress-reporter "Indenting buffer")))
        (if (bound-and-true-p lsp-mode)
            (lsp-format-buffer)
          (config-editing-indent-buffer))
        (progress-reporter-done progress))))))

(define-key prog-mode-map (kbd "M-q") #'config-indent-dwim)


;; Define a command for reversing the characters in the current region.

(defun config-reverse-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

(unless (fboundp 'reverse-characters)
  (defalias 'reverse-characters #'config-reverse-characters))



;; Provides nice commands for changing text scale for all buffers simultaneously.

(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-decrease default-text-scale-reset)
  :custom
  ((default-text-scale-amount 30)))


;; Teach Emacs to respect editorconfig files.

(use-package editorconfig
  :preface
  (autoload 'editorconfig-core-get-properties-hash "editorconfig-core")
  :config
  (editorconfig-mode 1))


;; Configure `conf-mode' for use with more kinds of config files.

(use-package conf-mode
  :mode
  (("\\.env\\.erb\\'" . conf-mode)
   ("\\.conf\\.erb\\'" . conf-mode)
   ("\\.kll\\'" . conf-mode)))


;; `align' provides useful functions for aligning text.

(use-package align
  :general ("C-x a a" #'align-regexp))


;; `hexl' is Emacs' built-in hex editor.

(use-package hexl
  :defer t
  :general
  (:states 'motion :keymaps 'hexl-mode-map
   "]]" #'hexl-end-of-1k-page
   "[[" #'hexl-beginning-of-1k-page
   "h" #'hexl-backward-char
   "l" #'hexl-forward-char
   "j" #'hexl-next-line
   "k" #'hexl-previous-line
   "$" #'hexl-end-of-line
   "^" #'hexl-beginning-of-line
   "0" #'hexl-beginning-of-line)
  :config
  (major-mode-hydra-define hexl-mode nil
    ("Insert"
     (("d" hexl-insert-decimal-char "decimal")
      ("c" hexl-insert-octal-char "octal")
      ("x" hexl-insert-hex-char "hex")
      ("s" hexl-insert-hex-string "hex string"))
     "Address"
     (("g" hexl-goto-address "goto")))))

;; `aggressive-indent' automatically reindents code during editing.

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :disabled t
  :hook (prog-mode . (lambda () (require 'aggressive-indent)))
  :init
  (general-setq aggressive-indent-excluded-modes
                '(csharp-mode
                  diff-auto-refine-mode
                  dockerfile-mode
                  fstar-mode
                  graphviz-dot-mode
                  haskell-mode
                  haskell-cabal-mode
                  idris-mode
                  idris-repl-mode
                  inf-ruby-mode
                  java-mode
                  lua-mode
                  makefile-gmake-mode
                  makefile-mode
                  nix-mode
                  python-mode
                  restclient-mode
                  rust-mode
                  scala-mode
                  sql-mode
                  stylus-mode
                  terraform-mode
                  text-mode
                  toml-mode
                  yaml-mode))

  :preface
  (defun turn-off-aggressive-indent-mode ()
    (when (fboundp 'aggressive-indent-mode)
      (aggressive-indent-mode -1)))

  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook #'turn-off-aggressive-indent-mode)
    (global-aggressive-indent-mode +1)))


;; `volatile-highlights' shows highlights in the buffer when regions change.

(use-package volatile-highlights
  :hook ((prog-mode . (lambda () (require 'volatile-highlights)))
         (text-mode . (lambda () (require 'volatile-highlights))))

  :commands (volatile-highlights-mode)
  :functions (vhl/install-extension
              vhl/define-extension
              vhl/load-extension)

  :config
  (progn

    ;; Evil support.

    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)

    (with-eval-after-load 'evil
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))

    (volatile-highlights-mode)))


;; `tiny' provides a template syntax for creating sequences of text.

(use-package tiny
  :general ("C-:" #'tiny-expand))


;; `ws-butler' cleans up trailing whitespace as you edit.

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :hook ((prog-mode . (lambda () (require 'ws-butler)))
         (text-mode . (lambda () (require 'ws-butler))))
  :config
  (ws-butler-global-mode))


;; `unfill' provides a command that is the opposite of fill-paragraph.

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))


;; `hide-comnt' provides a command for hiding comments.

(use-package hide-comnt
  :commands (hide/show-comments-toggle))


;; `highlight-thing' highlights the symbol at point.

(use-package highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :preface
  (progn
    (defun config-editing--face-ancestors (face)
      (let (result)
        (while (and face (not (equal face 'unspecified)))
          (setq result (cons face result))
          (setq face (face-attribute face :inherit)))
        (nreverse result)))

    (defun config-editing--should-highlight-p (res)
      (unless (bound-and-true-p lsp-ui-mode)
        (when res
          (let ((excluded-faces '(font-lock-string-face
                                  font-lock-keyword-face
                                  font-lock-comment-face
                                  font-lock-preprocessor-face
                                  font-lock-builtin-face))
                (faces (seq-mapcat #'config-editing--face-ancestors (face-at-point nil t))))
            (null (seq-intersection faces excluded-faces)))))))

  :config
  (progn
    (set-face-attribute 'highlight-thing nil :inherit 'highlight)

    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.1)
    (setq highlight-thing-limit-to-defun nil)
    (setq highlight-thing-case-sensitive-p t)

    (advice-add 'highlight-thing-should-highlight-p :filter-return
                #'config-editing--should-highlight-p)))


;; `auto-highlight-symbol' highlights the symbol at point after a short time.

(use-package auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode)
  :preface
  ;; HACK: Prevent the default keymap from getting created
  (defvar auto-highlight-symbol-mode-map (make-sparse-keymap))
  :config
  (progn
    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    (setq ahs-idle-timer 0)))


;; `dump-jump' provides a good fallback for navigating to definitions in the
;; absence of tags tables or semantic analysis.

(use-package dumb-jump
  :general (:states 'normal :keymaps 'prog-mode-map "M-." #'jump-to-definition)
  :config
  (progn
    (when (featurep 'ivy)
      (setq dumb-jump-selector 'ivy))

    (add-to-list 'dumb-jump-language-file-exts '(:language "javascript" :ext "ts" :agtype "js" :rgtype "ts"))))


;; `autoinsert' provides file templates.

(use-package autoinsert
  :preface
  (defvar auto-insert-alist nil)
  :hook (find-file . auto-insert)
  :config
  (setq auto-insert-query nil))

(use-package autoinsert-files
  :after autoinsert
  :preface
  (progn
    ;; Use yasnippet's `snippet-mode' for autoinsert templates
    (autoload 'snippet-mode "yasnippet")

    (defun config-editing--maybe-snippet-mode ()
      (require 'autoinsert)
      (when (f-descendant-of-p (buffer-file-name) auto-insert-directory)
        (snippet-mode))))

  :init
  (add-hook 'find-file-hook #'config-editing--maybe-snippet-mode)
  :config
  (advice-add 'auto-insert :before (lambda (&rest _)
                                     (autoinsert-files-populate-templates))))

;; `deadgrep' provides a polished frontend for `ripgrep'.

(use-package deadgrep
  :commands (deadgrep)
  :general (:keymaps 'deadgrep-mode-map "C-c C-w" #'deadgrep-edit-mode)
  :init
  (defalias 'rg #'deadgrep)

  :preface
  (defun config-editing--deadgrep-requery ()
    (interactive)
    (let ((button (save-excursion
                    (goto-char (point-min))
                    (forward-button 1))))
      (button-activate button)))
  :general (:states 'normal :keymaps 'deadgrep-mode-map "c" #'config-editing--deadgrep-requery)

  :preface
  (defun config-editing--on-enter-deadgrep-edit-mode (&rest _)
    (message "Entering edit mode. Changes will be made to underlying files as you edit."))
  :config
  (advice-add #'deadgrep-edit-mode :after #'config-editing--on-enter-deadgrep-edit-mode)

  :preface
  (defun config-editing--on-exit-deadgrep-edit-mode (&rest _)
    (when (derived-mode-p 'deadgrep-edit-mode)
      (message "Exiting edit mode.")))
  :config
  (advice-add #'deadgrep-mode :before #'config-editing--on-exit-deadgrep-edit-mode)

  :preface
  (defun deadgrep-from-ivy ()
    (interactive)
    (ivy-exit-with-action
     (lambda (&rest _)
       (let ((deadgrep--search-type 'regexp))
         (deadgrep (replace-regexp-in-string (rx (+ space)) ".*?" ivy-text))))))
  :init
  (general-define-key :keymaps 'counsel-ag-map "C-c C-e" #'deadgrep-from-ivy))

;; `wgrep' provides a mode for editing files directly from grep buffers.

(use-package wgrep
  :defer t)

;; `flyspell' provides spellchecking.

(use-package flyspell
  :hook
  ((org-mode . flyspell-mode))
  :custom
  ((flyspell-issue-welcome-flag nil)
   (flyspell-default-dictionary "en_GB")))

;; `rotate' provides handy commands for manipulating the window layout.

(use-package rotate
  :commands (rotate-layout))

;; `annotate' provides an interface for adding annotations to files without
;; modifying them.

(use-package annotate
  :commands (annotate-annotate)
  :demand t
  :custom
  ((annotate-blacklist-major-mode '(org-agenda-mode org-mode special-mode))
   (annotate-use-messages nil)))

;; `literate-calc-mode' adds support for inline formulas and shows the results
;; with overlays.

(use-package literate-calc-mode
  :commands (literate-calc-minor-mode)
  :hook ((org-mode . config-editing--maybe-literate-calc-minor-mode))
  :preface
  (defun config-editing--maybe-literate-calc-minor-mode ()
    (unless (and (buffer-file-name)
                 (string-match-p "archive.org\\'" (buffer-file-name)))
      (literate-calc-minor-mode)))
  :config
  ;; Don't run flyspell on tokens in literate-calc-mode expressions.
  (defun config-editing--ad-flyspell-compat (f &rest args)
    (and (apply f args)
         (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
           (not (string-match-p literate-calc--expression line))))))

(provide 'config-editing)

;;; config-editing.el ends here
