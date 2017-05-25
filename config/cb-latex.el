;;; cb-latex.el --- Configuration for latex.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)


(use-package latex-preview-pane
  :commands (latex-preview-pane-update))


;; Auctex

(defvar cb-latex-command "LaTeX")

(use-package tex
  :defer t
  :preface
  (defvar-local TeX-syntactic-comments t)
  :config
  (progn
    (setq TeX-command-default cb-latex-command)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    ;; Synctex support
    (setq TeX-source-correlate-start-server nil)))

(use-package latex
  :after tex
  :preface
  (progn
    (autoload 'TeX-save-document "tex-buf")
    (autoload 'TeX-command "tex-buf")
    (autoload 'TeX-master-file "tex")
    (autoload 'LaTeX-current-environment "latex")

    (defvar TeX-save-query)

    (defun cb-latex-build ()
      (interactive)
      (progn
        (let ((TeX-save-query nil))
          (TeX-save-document (TeX-master-file)))
        (TeX-command cb-latex-command 'TeX-master-file -1)))

    (defvar cb-latex-no-indent-envs '("equation" "equation*" "align" "align*" "tabular" "tikzpicture"))

    (defun cb-latex--autofill ()
      ;; Check whether the pointer is currently inside one of the
      ;; environments described in `cb-latex-no-indent-envs' and if so, inhibits
      ;; the automatic filling of the current paragraph.
      (let ((env)
            (should-fill t)
            (level 0))
        (while (and should-fill (not (equal env "document")))
          (setq level (1+ level))
          (setq env (LaTeX-current-environment level))
          (setq should-fill (not (member env cb-latex-no-indent-envs))))

        (when should-fill
          (do-auto-fill))))

    (defun cb-latex--auto-fill-mode ()
      (auto-fill-mode +1)
      (setq-local auto-fill-function #'cb-latex--autofill))

    ;; Rebindings for TeX-font.
    (defun cb-latex-font-bold () (interactive) (TeX-font nil ?\C-b))
    (defun cb-latex-font-code () (interactive) (TeX-font nil ?\C-t))
    (defun cb-latex-font-emphasis () (interactive) (TeX-font nil ?\C-e))
    (defun cb-latex-font-italic () (interactive) (TeX-font nil ?\C-i))
    (defun cb-latex-font-medium () (interactive) (TeX-font nil ?\C-m))
    (defun cb-latex-font-clear () (interactive) (TeX-font nil ?\C-d))
    (defun cb-latex-font-calligraphic () (interactive) (TeX-font nil ?\C-a))
    (defun cb-latex-font-small-caps () (interactive) (TeX-font nil ?\C-c))
    (defun cb-latex-font-sans-serif () (interactive) (TeX-font nil ?\C-f))
    (defun cb-latex-font-normal () (interactive) (TeX-font nil ?\C-n))
    (defun cb-latex-font-serif () (interactive) (TeX-font nil ?\C-r))
    (defun cb-latex-font-oblique () (interactive) (TeX-font nil ?\C-s))
    (defun cb-latex-font-upright () (interactive) (TeX-font nil ?\C-u)))

  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'cb-latex--auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

  :config
  (progn
    ;; Don't insert line-break at inline math.
    (setq LaTeX-fill-break-at-separators nil)

    (dolist (mode '(tex-mode latex-mode))
      (spacemacs-keys-set-leader-keys-for-major-mode mode
        "\\"  'TeX-insert-macro                            ;; C-c C-m
        "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
        ";"   'TeX-comment-or-uncomment-region             ;; C-c ; or C-c :
        ;; TeX-command-run-all runs compile and open the viewer
        "a"   'TeX-command-run-all                         ;; C-c C-a
        "b"   'cb-latex-build
        "k"   'TeX-kill-job                                ;; C-c C-k
        "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "m"   'TeX-insert-macro                            ;; C-c C-m
        "v"   'TeX-view                                    ;; C-c C-v
        ;; TeX-doc is a very slow function
        "hd"  'TeX-doc
        "xb"  'cb-latex-font-bold
        "xc"  'cb-latex-font-code
        "xe"  'cb-latex-font-emphasis
        "xi"  'cb-latex-font-italic
        "xr"  'cb-latex-font-clear
        "xo"  'cb-latex-font-oblique
        "xfc" 'cb-latex-font-small-caps
        "xff" 'cb-latex-font-sans-serif
        "xfr" 'cb-latex-font-serif))

    (spacemacs-keys-set-leader-keys-for-major-mode 'latex-mode
      "*"   'LaTeX-mark-section      ;; C-c *
      "."   'LaTeX-mark-environment  ;; C-c .
      "c"   'LaTeX-close-environment ;; C-c ]
      "e"   'LaTeX-environment       ;; C-c C-e
      "ii"   'LaTeX-insert-item       ;; C-c C-j
      "s"   'LaTeX-section           ;; C-c C-s
      "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
      "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
      "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
      "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
      "p"   'latex-preview-pane-update
      "xB"  'cb-latex-font-medium
      "xr"  'cb-latex-font-clear
      "xfa" 'cb-latex-font-calligraphic
      "xfn" 'cb-latex-font-normal
      "xfu" 'cb-latex-font-upright)

    (spacemacs-keys-declare-prefix-for-mode 'latex-mode "mi" "insert")
    (spacemacs-keys-declare-prefix-for-mode 'latex-mode "mf" "fill")))

(use-package tex-fold
  :defer t
  :config
  (dolist (mode '(tex-mode latex-mode))
    (spacemacs-keys-set-leader-keys-for-major-mode mode
      "z=" 'TeX-fold-math
      "zb" 'TeX-fold-buffer
      "zB" 'TeX-fold-clearout-buffer
      "ze" 'TeX-fold-env
      "zI" 'TeX-fold-clearout-item
      "zm" 'TeX-fold-macro
      "zp" 'TeX-fold-paragraph
      "zP" 'TeX-fold-clearout-paragraph
      "zr" 'TeX-fold-region
      "zR" 'TeX-fold-clearout-region
      "zz" 'TeX-fold-dwim)))


(provide 'cb-latex)

;;; cb-latex.el ends here
