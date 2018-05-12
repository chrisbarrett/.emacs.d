;;; config-latex.el --- Configuration for latex.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'spacemacs-keys)

(use-package latex-preview-pane
  :straight t
  :commands (latex-preview-pane-mode))

;; Auctex

(defvar config-latex--command "LaTeX")

(use-package which-key
  :config
  (progn
    (push `((nil . ,(rx bos (? "config-") (? "la") "tex-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)
    (push `((nil . ,(rx bos (? "La") "TeX-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package tex
  :straight auctex
  :preface
  (defvar-local TeX-syntactic-comments t)
  :config
  (progn
    (setq TeX-command-default config-latex--command)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    ;; Synctex support
    (setq TeX-source-correlate-start-server nil)))

(use-package latex
  :straight auctex
  :defer t
  :preface
  (progn
    (autoload 'LaTeX-current-environment "latex")
    (autoload 'TeX-command "tex-buf")
    (autoload 'TeX-font "tex")
    (autoload 'TeX-master-file "tex")
    (autoload 'TeX-save-document "tex-buf")

    (defvar TeX-save-query)

    (defun config-latex-build ()
      (interactive)
      (progn
        (let ((TeX-save-query nil))
          (TeX-save-document (TeX-master-file)))
        (TeX-command config-latex--command 'TeX-master-file -1)))

    (defvar config-latex-no-indent-envs '("equation" "equation*" "align" "align*" "tabular" "tikzpicture"))

    (defun config-latex--autofill ()
      ;; Check whether the pointer is currently inside one of the
      ;; environments described in `config-latex-no-indent-envs' and if so, inhibits
      ;; the automatic filling of the current paragraph.
      (let ((env)
            (should-fill t)
            (level 0))
        (while (and should-fill (not (equal env "document")))
          (setq level (1+ level))
          (setq env (LaTeX-current-environment level))
          (setq should-fill (not (member env config-latex-no-indent-envs))))

        (when should-fill
          (do-auto-fill))))

    (defun config-latex--auto-fill-mode ()
      (auto-fill-mode +1)
      (setq-local auto-fill-function #'config-latex--autofill))

    ;; Rebindings for TeX-font.
    (defun config-latex-font-bold () (interactive) (TeX-font nil ?\C-b))
    (defun config-latex-font-code () (interactive) (TeX-font nil ?\C-t))
    (defun config-latex-font-emphasis () (interactive) (TeX-font nil ?\C-e))
    (defun config-latex-font-italic () (interactive) (TeX-font nil ?\C-i))
    (defun config-latex-font-medium () (interactive) (TeX-font nil ?\C-m))
    (defun config-latex-font-clear () (interactive) (TeX-font nil ?\C-d))
    (defun config-latex-font-calligraphic () (interactive) (TeX-font nil ?\C-a))
    (defun config-latex-font-small-caps () (interactive) (TeX-font nil ?\C-c))
    (defun config-latex-font-sans-serif () (interactive) (TeX-font nil ?\C-f))
    (defun config-latex-font-normal () (interactive) (TeX-font nil ?\C-n))
    (defun config-latex-font-serif () (interactive) (TeX-font nil ?\C-r))
    (defun config-latex-font-oblique () (interactive) (TeX-font nil ?\C-s))
    (defun config-latex-font-upright () (interactive) (TeX-font nil ?\C-u)))

  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'config-latex--auto-fill-mode)
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
        "b"   'config-latex-build
        "k"   'TeX-kill-job                                ;; C-c C-k
        "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "m"   'TeX-insert-macro                            ;; C-c C-m
        "v"   'TeX-view                                    ;; C-c C-v
        ;; TeX-doc is a very slow function
        "hd"  'TeX-doc
        "xb"  'config-latex-font-bold
        "xc"  'config-latex-font-code
        "xe"  'config-latex-font-emphasis
        "xi"  'config-latex-font-italic
        "xr"  'config-latex-font-clear
        "xo"  'config-latex-font-oblique
        "xfc" 'config-latex-font-small-caps
        "xff" 'config-latex-font-sans-serif
        "xfr" 'config-latex-font-serif))

    (spacemacs-keys-set-leader-keys-for-major-mode 'latex-mode
      "*"   'LaTeX-mark-section      ;; C-c *
      "."   'LaTeX-mark-environment  ;; C-c .
      "c"   'LaTeX-close-environment ;; C-c ]
      "e"   'LaTeX-environment       ;; C-c C-e
      "ii"  'LaTeX-insert-item       ;; C-c C-j
      "s"   'LaTeX-section           ;; C-c C-s
      "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
      "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
      "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
      "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
      "p"   'latex-preview-pane-mode
      "xB"  'config-latex-font-medium
      "xr"  'config-latex-font-clear
      "xfa" 'config-latex-font-calligraphic
      "xfn" 'config-latex-font-normal
      "xfu" 'config-latex-font-upright)

    (spacemacs-keys-declare-prefix-for-mode 'latex-mode "mi" "insert")
    (spacemacs-keys-declare-prefix-for-mode 'latex-mode "mf" "fill")))

(use-package tex-fold
  :straight auctex
  :after tex
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

(use-package company-auctex
  :straight t
  :hook (tex-mode . company-auctex-init))

(provide 'config-latex)

;;; config-latex.el ends here
