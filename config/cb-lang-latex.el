;;; cb-lang-latex.el --- Configuration for TeX and LaTeX.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Use `auctex' as the Tex and Latex editing mode.



;; `auctex' is disgusting and clobbers the builtin Tex modes. To load it lazily,
;; intercept attempts to load Tex files and make sure `auctex' is loaded first.

(defun cb-lazy-load-auctex ()
  (when (string-match-p (rx "." (or "latex" "tex") string-end)
                        (buffer-name))
    (require 'tex-site)))

(add-hook 'find-file-hook #'cb-lazy-load-auctex)



(use-package tex
  :preface
  (defvar-local TeX-syntactic-comments t)
  :custom
  (TeX-command (getenv "NIX_EMACS_TEX_PROGRAM"))
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-source-correlate-start-server nil)
  ;; Use Emacs pdf-tools as viewer.
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))

(use-package latex
  :custom
  (LaTeX-command (getenv "NIX_EMACS_TEX_PROGRAM"))
  ;; Don't insert line-break at inline math.
  (LaTeX-fill-break-at-separators nil)
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)


  ;; Use C-c C-c to build the current buffer with tectonic.

  :preface
  (autoload 'TeX-command "tex-buf")
  (autoload 'TeX-master-file "tex")
  (autoload 'TeX-save-document "tex-buf")
  (defvar TeX-save-query)

  (defun cb-latex-build ()
    (interactive)
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command (getenv "NIX_EMACS_TEX_PROGRAM") 'TeX-master-file -1))

  :general
  (:keymaps 'LaTeX-mode-map "C-c C-b" #'cb-latex-build)

  ;; Teach the autofill function in Latex buffers not to fill in certain
  ;; contexts.

  :preface
  (defvar cb-latex-no-indent-envs '("equation" "equation*" "align"
                                    "align*" "tabular" "tikzpicture"))

  (defun config-latex--autofill ()
    ;; Check whether the pointer is currently inside one of the environments
    ;; described in `cb-latex-no-indent-envs' and if so, inhibits the automatic
    ;; filling of the current paragraph.
    (let ((env)
          (should-fill t)
          (level 0))
      (while (and should-fill (not (equal env "document")))
        (cl-incf level)
        (setq env (LaTeX-current-environment level))
        (setq should-fill (not (member env cb-latex-no-indent-envs))))
      (when should-fill
        (do-auto-fill))))

  (defun cb-configure-latex-autofill ()
    (auto-fill-mode +1)
    (setq-local auto-fill-function #'config-latex--autofill))
  :init
  (add-hook 'LaTeX-mode-hook 'cb-configure-latex-autofill))

(use-package tex-fold
  :after tex
  :demand t)

(use-package latex-preview-pane
  :general (:keymaps 'LaTeX-mode-map "C-c p" #'latex-preview-pane))

(provide 'cb-lang-latex)

;;; cb-lang-latex.el ends here
