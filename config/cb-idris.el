;;; cb-idris.el --- Configuration for Idris.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)
(require 'evil)

(use-package idris-mode
  :mode ("\\.l?idr\\'" . idris-mode)
  :commands (idris-repl)
  :init
  (add-to-list 'completion-ignored-extensions ".ibc")
  :config
  (progn
    (setq idris-semantic-source-highlighting nil)

    (evil-define-key 'normal idris-mode-map (kbd "K") #'idris-docs-at-point)

    (spacemacs-keys-declare-prefix-for-mode 'idris-mode "mb" "build")
    (spacemacs-keys-declare-prefix-for-mode 'idris-mode "mi" "editing")
    (spacemacs-keys-declare-prefix-for-mode 'idris-mode "mh" "documentation")
    (spacemacs-keys-declare-prefix-for-mode 'idris-mode "ms" "repl")
    (spacemacs-keys-declare-prefix-for-mode 'idris-mode "mm" "term")

    (spacemacs-keys-set-leader-keys-for-major-mode 'idris-mode
      ;; Shorthands: rebind the standard evil-mode combinations to the local
      ;; leader for the keys not used as a prefix below.
      "c" 'idris-case-dwim
      "d" 'idris-add-clause
      "l" 'idris-make-lemma
      "p" 'idris-proof-search
      "r" 'idris-load-file
      "t" 'idris-type-at-point
      "w" 'idris-make-with-block

      ;; ipkg.
      "bc" 'idris-ipkg-build
      "bC" 'idris-ipkg-clean
      "bi" 'idris-ipkg-install
      "bp" 'idris-open-package-file

      ;; Interactive editing.
      "ia" 'idris-proof-search
      "ic" 'idris-case-dwim
      "ie" 'idris-make-lemma
      "im" 'idris-add-missing
      "ir" 'idris-refine
      "is" 'idris-add-clause
      "iw" 'idris-make-with-block

      ;; Documentation.
      "ha" 'idris-apropos
      "hd" 'idris-docs-at-point
      "hs" 'idris-type-search
      "ht" 'idris-type-at-point

      ;; Active term manipulations.
      "mn" 'idris-normalise-term
      "mi" 'idris-show-term-implicits
      "mh" 'idris-hide-term-implicits
      "mc" 'idris-show-core-term

      ;; REPL
      "'"  'idris-repl
      "sb" 'idris-load-file
      "si" 'idris-repl
      "sn" 'idris-load-forward-line
      "sp" 'idris-load-backward-line
      "ss" 'idris-pop-to-repl
      "sq" 'idris-quit)

    ;; Enable keyboard control for buttons in holes list.

    (dolist (state '(normal motion))
      (eval `(evil-define-key ',state idris-hole-list-mode-map
               (kbd "TAB")       #'forward-button
               (kbd "<backtab>") #'backward-button
               (kbd "RET")       #'button-activate)))

    ;; Customise prover keybindings.

    (dolist (state '(normal insert motion))
      (eval `(evil-define-key ',state idris-prover-script-mode-map
               (kbd "M-n")       'idris-prover-script-forward
               (kbd "M-p")       'idris-prover-script-backward)))

    (spacemacs-keys-set-leader-keys-for-major-mode 'idris-prover-script-mode
      "n" 'idris-prover-script-forward
      "p" 'idris-prover-script-backward
      "x" 'idris-prover-script-qed
      "q" 'idris-prover-abandon)

    (define-key idris-prover-script-mode-map (kbd "C-c C-k") 'idris-prover-abandon)
    (define-key idris-prover-script-mode-map (kbd "C-c C-c") 'idris-prover-script-qed)

    ;; Open special buffers in motion state so they can be closed with `q'.

    (evil-set-initial-state 'idris-compiler-notes-mode 'motion)
    (evil-set-initial-state 'idris-hole-list-mode 'motion)
    (evil-set-initial-state 'idris-info-mode 'motion)

    ;; Configure Idris window management.

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*idris-" (or "notes" "holes" "info") "*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.4)))))

(provide 'cb-idris)

;;; cb-idris.el ends here
