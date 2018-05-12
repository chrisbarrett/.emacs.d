;;; config-editing.el --- Config for simple editing packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)



(use-package aggressive-indent
  :straight t
  :commands (global-aggressive-indent-mode)
  :defer 3
  :init
  (defconst aggressive-indent-excluded-modes
    '(cb-scala-sbt-file-mode
      cb-web-js-mode
      diff-auto-refine-mode
      dockerfile-mode
      fstar-mode
      graphviz-dot-mode
      haskell-mode
      idris-mode
      idris-repl-mode
      inf-ruby-mode
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



(use-package volatile-highlights
  :straight t
  :defer 3

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

    ;; Undo-tree support.

    (vhl/define-extension 'undo-tree
                          'undo-tree-move
                          'undo-tree-yank)

    (with-eval-after-load 'undo-tree
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))

    (volatile-highlights-mode)))

(use-package tiny
  :straight t
  :bind ("C-:" . tiny-expand))

(use-package undo-tree
  :straight t
  :demand t
  :commands (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (global-undo-tree-mode)))

(use-package ws-butler
  :straight t
  :commands (ws-butler-global-mode)
  :defer 3
  :config
  (ws-butler-global-mode))

(use-package highlight-thing
  :straight t
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
      "Do not highlight symbol if looking at certain faces."
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'config-editing--face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))

  :config
  (progn
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun nil)
    (setq highlight-thing-case-sensitive-p t)

    (advice-add 'highlight-thing-should-highlight-p :filter-return
                #'config-editing--should-highlight-p)))

(use-package unfill
  :straight t
  :commands (unfill-region unfill-paragraph unfill-toggle))



;; Prevent the default keymap from getting created
(defvar auto-highlight-symbol-mode-map (make-sparse-keymap))

(use-package auto-highlight-symbol
  :straight t
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (progn
    (setq ahs-case-fold-search nil)
    (setq ahs-default-range 'ahs-range-whole-buffer)
    (setq ahs-idle-interval 0.25)
    (setq ahs-inhibit-face-list nil)

    ;; Disable by default, use keybinding instead.
    (setq ahs-idle-timer 0)))

(use-package evil-ahs
  :commands (evil-ahs/highlight-symbol evil-ahs/goto-last-searched-symbol)
  :bind (:map
         evil-motion-state-map
         ("*" . evil-ahs/enter-ahs-forward)
         ("#" . evil-ahs/enter-ahs-backward)
         :map
         spacemacs-keys-default-map
         ("sh" . evil-ahs/highlight-symbol)
         ("sH" . evil-ahs/goto-last-searched-symbol)))

(provide 'config-editing)

;;; config-editing.el ends here
