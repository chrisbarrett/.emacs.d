;;; config-latex.el --- Configuration for latex.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'straight)



(cb-major-mode-hydra-define latex-mode
  "Build"
  (("r" TeX-command-run-all "run")
   ("b" config-latex-build "build")
   ("o" TeX-view "open output"))

  "Insert"
  (("ie" LaTeX-environment "environment")
   ("ic" LaTeX-close-environment "close environemnt")
   ("ii" LaTeX-insert-item "item")
   ("is" LaTeX-section "section")
   ("im" TeX-insert-macro "macro"))

  "Select"
  (("vs" LaTeX-mark-section "section")
   ("ve" LaTeX-mark-environment "environment"))

  "Fill"
  (("fe" LaTeX-fill-environment "environment")
   ("fp" LaTeX-fill-paragraph "paragram")
   ("fr" LaTeX-fill-region "region")
   ("fs" LaTeX-fill-section "section"))

  "Markup"
  (("mb" (TeX-font nil ?\C-b) "bold")
   ("mc" (TeX-font nil ?\C-t) "code")
   ("me" (TeX-font nil ?\C-e) "emphasis")
   ("mi" (TeX-font nil ?\C-i) "italic"))

  "Misc"
  (("p" latex-preview-pane-mode "toggle preview pane")
   ("h" TeX-doc "documentation")))



;; Auctex is disgusting and clobbers the builtin tex modes with its wacky
;; loading process. It doesn't even have the good grace to provide itself as a
;; feature.

(defun config-latex-lazy-load-auctex ()
  (when (string-match-p (rx "." (or "latex" "tex") string-end)
                        (buffer-name))
    (unless (featurep 'auctex-load-hack)
      (require 'auctex-load-hack)
      (revert-buffer nil t))))

(add-hook 'find-file-hook #'config-latex-lazy-load-auctex)



(defvar config-latex--command "LaTeX")

(use-package tex
  :straight (auctex :host github :repo "raxod502/auctex"
                    :branch "fork/1"
                    :files (:defaults (:exclude "doc/*.texi")))
  :defer t
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
      (setq-local auto-fill-function #'config-latex--autofill)))

  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'config-latex--auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

  :config
  ;; Don't insert line-break at inline math.
  (setq LaTeX-fill-break-at-separators nil))

(use-package tex-fold
  :after tex)

(use-package company-auctex
  :straight t
  :hook (tex-mode . company-auctex-init))

(use-package latex-preview-pane
  :straight t
  :commands (latex-preview-pane-mode))

(provide 'config-latex)

;;; config-latex.el ends here
