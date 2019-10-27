;;; config-latex.el --- Configuration for latex.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general)
(require 'major-mode-hydra)
(require 'memoize)

(major-mode-hydra-define latex-mode nil
  ("Build"
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
    ("h" TeX-doc "documentation"))))

;; Auctex is disgusting and clobbers the builtin tex modes with its wacky
;; loading process. To make the Auctex loading process work, we load Auctex the
;; first time we visit a tex file, then re-open it after the tex modes have been
;; redefined.

(autoload 'TeX-load-hack (expand-file-name "straight/build/auctex/tex-site.el" user-emacs-directory))

(defmemoize config-latex--load-auctex-once ()
  (TeX-load-hack)
  (revert-buffer nil t)
  t)

(defun config-latex-lazy-load-auctex ()
  (when (string-match-p (rx "." (or "latex" "tex") string-end)
                        (buffer-name))
    (config-latex--load-auctex-once)))

(add-hook 'find-file-hook #'config-latex-lazy-load-auctex)



(defvar config-latex--command "LaTeX")

(use-package tex-site
  :straight (auctex)
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
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'config-latex--auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

  :config
  (general-setq
   ;; Don't insert line-break at inline math.
   LaTeX-fill-break-at-separators nil

   ;; Use Emacs pdf-tools as viewer.
   TeX-view-program-selection '((output-pdf "PDF Tools"))
   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))

(use-package tex-fold
  :defer t
  :after tex)

(use-package company-auctex
  :straight t
  :defer t
  :hook (tex-mode . company-auctex-init))

(use-package latex-preview-pane
  :straight t
  :defer t
  :commands (latex-preview-pane-mode))

(provide 'config-latex)

;;; config-latex.el ends here
