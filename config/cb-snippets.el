;;; cb-snippets.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'thingatpt)
(require 's)

(use-package autoinsert :hook (find-file . auto-insert)
  :preface
  (defvar auto-insert-alist nil)
  :custom
  (auto-insert-query nil))

;; Extend `auto-insert' to use the more intuitive `yasnippet' DSL.

(use-package autoinsert-files :demand t :after autoinsert
  :disabled t
  :autoload (autoinsert-files-populate-templates)
  :preface
  (autoload 'snippet-mode "yasnippet")

  (defun cb-autoinsert-maybe-enter-snippet-mode ()
    (when (and (bound-and-true-p auto-insert-directory)
               (string-prefix-p auto-insert-directory (buffer-file-name)))
      (snippet-mode)))
  :hook
  (find-file . #'cb-autoinsert-maybe-enter-snippet-mode)
  :config
  (define-advice auto-insert (:before ())
    (autoinsert-files-populate-templates)))



(use-package yasnippet :ensure t :demand t
  :config
  (yas-global-mode +1)

  :custom
  (yas-wrap-around-region t)
  (yas-alias-to-yas/prefix-p nil)
  (yas-prompt-functions '(yas-completing-prompt))
  (yas-verbosity 0)
  (yas-minor-mode-map (make-sparse-keymap))

  :general
  (:keymaps 'yas-minor-mode-map :states 'insert
   "TAB"
   (general-predicate-dispatch 'indent-for-tab-command
     (yas-maybe-expand-abbrev-key-filter t) 'yas-expand))
  (:keymaps 'yas-keymap :states 'insert
   "SPC"
   (general-predicate-dispatch 'self-insert-command
     (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field)
   "<backspace>"
   (general-predicate-dispatch 'backward-delete-char
     (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field
     (bound-and-true-p smartparens-mode) 'sp-backward-delete-char))

  ;; Place point at the end of previous field when cycling backwards.
  :config
  (defun cb-yasnippet-goto-field-end (&rest _)
    (when-let* ((field (yas-current-field)))
      (when (and (yas--field-modified-p field)
                 (yas--field-contains-point-p field))
        (goto-char (marker-position (yas--field-end field)))))
    (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
      (evil-insert-state)))

  (advice-add 'yas-next-field :after #'cb-yasnippet-goto-field-end)
  (advice-add 'yas-prev-field :after #'cb-yasnippet-goto-field-end))

;; Don't warn if snippet modifies buffer.

;; I sometimes want to do just-one-space. There's no nice way to do this. The
;; hacky solution is to add an inline lisp call inline to `just-one-space'.

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(yasnippet))
  (add-to-list 'warning-suppress-log-types '(yasnippet)))



;;; Snippet functions

(defun cb-yas-bolp ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match-p (rx bol (* space) (* word) (* space) eol)
                    line)))

(defmacro cb-yas-line-rx-p (&rest rx-forms)
  `(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
     (string-match-p (rx-to-string '(and ,@rx-forms))
                     line)))

;; emacs-lisp

(defun yas-funcs-el-custom-group ()
  "Find the first group defined in the current file.

Fall back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun yas-funcs-el-autoload-file (sym)
  (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
      (file-name-sans-extension (file-name-nondirectory file))
    ""))

(defun yas-funcs-el-at-line-above-decl-p ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))

(defun yas-funcs-el-package-prefix ()
  (cond
   ((string-prefix-p "*Org Src" (buffer-name))
    "")
   ((bound-and-true-p nameless-current-name)
    (format "%s-" nameless-current-name))
   (t
    (format "%s-" (file-name-base (or (buffer-file-name) (buffer-name)))))))

(defun yas-funcs-buttercup-file-p ()
  (string-match-p "^test-" (file-name-nondirectory (buffer-file-name))))

;; csharp

(defun cb-csharp-expression-context-p (regexp)
  (thing-at-point-looking-at (rx-to-string `(and (or "=>"
                                                     (any ",;()=:"))
                                                 (* space) (regexp ,regexp) (* space)))))

(provide 'cb-snippets)

;;; cb-snippets.el ends here
