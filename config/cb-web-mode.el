;;; cb-web-mode.el --- Configuration for web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package web-mode
  :defines (web-mode-markup-indent-offset
            web-mode-css-indent-offset)

  :preface
  (autoload 'sp-local-pair "smartparens")

  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)

    ;; Treat es6 files as JS files.

    (add-to-list 'web-mode-content-types '("javascript" . "\\.es6\\'"))
    (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))))


(use-package smartparens
  :defer t
  :functions (sp-get-pair)
  :preface
  (progn
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 's-matches? "s")

    (defun cb-web--format-after-paren (_id action context)
      "Insert a space after some keywords."
      (when (and (equal action 'insert)
                 (equal context 'code)
                 (thing-at-point-looking-at
                  (rx symbol-start (or "=" "for" "of" "in"
                                       "if" "else" "while"
                                       "return"
                                       "yield" "yield*"
                                       "function" "function*")
                      (* space) "(")))
        (save-excursion
          (search-backward "(")
          (just-one-space))))

    (defun cb-web--sp-web-mode-is-code-context (_id action _context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (defun cb-web--sp-external-padding (id action ctx)
      "Add external padding around ID.
Insert leading padding unless at start of line or after an open round paren."
      (when (and (equal action 'insert)
                 (equal ctx 'code))
        (save-excursion
          (when (search-backward (sp-get-pair id :open)
                                 (line-beginning-position) t)
            (let ((bol-to-point (buffer-substring (line-beginning-position) (point))))
              (cond
               ((s-matches? (rx bol (* space) eol) bol-to-point))
               ((s-matches? (rx (or "(" "[") (* space) eol) bol-to-point)
                (delete-horizontal-space))
               (t
                (just-one-space))))
            t))))

    (defun cb-web--sp-braces-external-padding (id action ctx)
      (when (and (equal action 'insert)
                 (equal ctx 'code))
        (-when-let* ((end (point))
                     (beg (save-excursion
                            (search-backward (sp-get-pair id :open)
                                             (line-beginning-position) t))))
          (cond
           ((s-matches? (rx bol (* space) "<" (*? nonl) "=" (* space) eol)
                        (buffer-substring (line-beginning-position) beg))
            ;; Delete leading spaces
            (save-excursion
              (goto-char beg)
              (delete-horizontal-space))
            ;; Delete trailing spaces before end of tag marker.
            (save-excursion
              (forward-char)
              (skip-chars-forward " ")
              (when (equal (char-after) ?>)
                (delete-horizontal-space))))
           (t
            (cb-web--sp-external-padding id action ctx)))
          t))))

  :config
  (progn
    (sp-local-pair 'cb-web-js-mode "(" ")" :post-handlers '(:add cb-web--format-after-paren))
    (sp-local-pair '(cb-web-js-mode cb-web-json-mode) "{" "}" :post-handlers '(:add cb-web--sp-braces-external-padding))
    (sp-local-pair 'web-mode "<" nil :when '(cb-web--sp-web-mode-is-code-context))))


(use-package cb-web-modes
  :defer t
  :mode (("\\.json\\'" . cb-web-json-mode)
         ("\\.es6\\'"  . cb-web-js-mode)
         ("\\.jsx?\\'" . cb-web-js-mode)
         ("\\.html\\'" . cb-web-html-mode))
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'json-jsonlint 'cb-web-json-mode)))


(use-package flycheck
  :defer t
  :functions (flycheck-add-mode)
  :config
  (progn
    (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
    (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
    (flycheck-add-mode 'javascript-eslint 'cb-web-js-mode)))


(use-package emmet-mode
  :defer t
  :defines (emmet-expand-jsx-className?)
  :preface
  (defun cb-web--set-jsx-classname-on ()
    (setq-local emmet-expand-jsx-className? t))
  :config
  (add-hook 'cb-web-js-mode-hook #'cb-web--set-jsx-classname-on))


(provide 'cb-web-mode)

;;; cb-web-mode.el ends here
