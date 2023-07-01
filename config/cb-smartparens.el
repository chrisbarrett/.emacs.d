;;; cb-smartparens.el --- Configuration for smartparens.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code

(require 'cl-lib)



;;; Smartparens

(cl-eval-when (compile)
  (require 'smartparens))

(use-package smartparens
  :hook
  (prog-mode . smartparens-strict-mode)
  (text-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode +1)
  (show-smartparens-global-mode +1)

  :general
  (:keymaps 'smartparens-strict-mode-map
            [remap c-electric-backspace] #'sp-backward-delete-char)
  (:states 'insert
           ")" #'sp-up-sexp)
  (:states 'normal
           "D" #'sp-kill-hybrid-sexp)

  :custom
  (sp-show-pair-delay 0.2)
  (sp-show-pair-from-inside t)
  (sp-cancel-autoskip-on-backward-movement nil)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-navigate-close-if-unbalanced t)
  (sp-message-width nil)

  ;;; Utilities

  :autoload
  sp-pair
  sp-local-pair sp-get-pair
  sp--get-opening-regexp
  sp--get-closing-regexp

  :preface
  (defun config-smartparens-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (cond
         ((and (eq (preceding-char) ?$)
               (equal id "{")))

         ((eq (char-syntax (preceding-char)) ?w)
          (just-one-space))

         ((and (looking-back (sp--get-closing-regexp) (line-beginning-position))
               (not (eq (char-syntax (preceding-char)) ?')))
          (just-one-space))))))

  (defun config-smartparens-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  ;;; General pairs

  :config
  (sp-pair "`" "`"
           :bind "M-`")
  (sp-pair "{" "}"
           :bind "M-{"
           :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "[" "]"
           :bind "M-["
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "(" ")"
           :bind "M-("
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "\"" "\""
           :bind "M-\""
           :pre-handlers '(:add (config-smartparens-add-space-before-sexp-insertion)))

  ;;; Mode-specific

  :config
  (sp-with-modes '(org-mode markdown-mode gfm-mode latex-mode)
    ;; Don't pad curly-braces.
    (sp-local-pair "{" "}" :pre-handlers nil))

  (sp-with-modes (cons 'lisp-data-mode sp-lisp-modes)
    (sp-local-pair "(" nil
                   :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                   :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
    (sp-local-pair "[" nil
                   :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                   :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
    (sp-local-pair "\"" nil
                   :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                   :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
    (sp-local-pair "{" nil
                   :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                   :post-handlers '(config-smartparens-add-space-after-sexp-insertion)))

  (sp-with-modes '(oil-mode)
    (sp-local-pair "'''" "'''")
    (sp-local-pair "$'''" "'''")
    (sp-local-pair "\"\"\"" "\"\"\""))

  ;; Better checkboxes in org-mode

  :preface
  (autoload 'org-in-block-p "org")
  (defun config-smartparens--format-checkitem (_id action context)
    (when (and (eq action 'insert)
               (if (derived-mode-p 'org-mode)
                   (not (org-in-block-p org-list-forbidden-blocks))
                 t)
               (string-match-p (rx bos (* space) "-" (* space)
                                   (? "[" (* space) (? "]" (* space)))
                                   (* space)
                                   eos)
                               (buffer-substring (line-beginning-position)
                                                 (point))))
      (atomic-change-group
        (just-one-space)
        (search-backward "[" (line-beginning-position))
        (just-one-space)
        (search-forward "]" (line-end-position))
        (just-one-space))))

  :config
  (sp-with-modes '(org-mode markdown-mode gfm-mode)
    (sp-local-pair "[" "]" :post-handlers '(config-smartparens--format-checkitem)))

  ;; Whitespace formatting when deleting

  :autoload
  sp-get-enclosing-sexp
  :preface
  (define-advice sp-backward-delete-char (:around (f &rest args) cleanup)
    "Perform context-sensitive whitespace cleanups when deleting.

For performance, only consider a subset of the buffer."
    (save-restriction
      (unless (derived-mode-p 'emacs-lisp-mode)
        (apply #'narrow-to-region (bounds-of-surrounding-lines 500 500)))

      (-let* ((line-before-pt (buffer-substring (line-beginning-position) (point)))
              (line-after-pt (buffer-substring (point) (line-end-position)))

              ((&plist :beg beg :end end :op op :cl cl) (sp-get-enclosing-sexp))
              (inside-start (when op (+ beg (length op))))
              (inside-end   (when op (- end (length cl))))
              (inside       (when op
                              (concat (buffer-substring inside-start (point))
                                      (buffer-substring (point) inside-end)))))
        (cond
         ;; Collapse horizontal space in empty pairs.
         ;;
         ;; [  |  ] -> [|]
         ;;
         ((when op (string-match-p (rx bos (+ space) eos) inside))
          (delete-region inside-start inside-end))

         ;; Delete contents for multiline pairs that were just inserted, e.g. braces.
         ;;
         ;; {
         ;;   |
         ;; }
         ;;
         ;; ->
         ;;
         ;; {|}
         ((when op (string-match-p (rx bos (* space) "\n" (* space) "\n" (* space) eos) inside))
          (delete-region inside-start inside-end))

         ;; Delete back from end of the line.
         ;;
         ;;
         ;; foo |
         ;; ->
         ;; foo|

         ;; foo      |
         ;; ->
         ;; foo |
         ((string-empty-p line-after-pt)
          (if (string-match-p (rx space space eos) line-before-pt)
              (while (looking-back (rx space space) (line-beginning-position))
                (delete-char -1))
            (funcall f args)))

         ;; Don't aggressively delete whitespace if there's a comment
         ;; following pt.
         ;;
         ;;
         ;; foo |  // bar
         ;;
         ;; ->
         ;;
         ;; foo|  // bar
         ;;
         ((string-match-p (rx (* nonl) (syntax comment-start)) line-after-pt)
          (funcall f args))

         ;; Collapse surrounding space, but preserve padding inside pairs.
         ;;
         ;; foo | bar -> foo|bar
         ;;
         ;; foo | }   -> foo| }
         ;;
         ((and (string-match-p (rx (or bol (not space)) space eos) line-before-pt)
               (string-match-p (rx bos space (or eol (not space))) line-after-pt))
          (let ((backward-only? (when inside (string-match-p (rx bos space) inside))))
            (delete-horizontal-space backward-only?)))

         ;; Delete if there is a single preceding space.
         ;;
         ;; foo |bar -> foo|bar
         ;;
         ;; but not:
         ;;
         ;; foo| bar -> foo|bar
         ;;
         ((and (string-match-p (rx (or bol (not space)) space eos) line-before-pt)
               (string-match-p (rx bos (not space)) line-after-pt))
          (delete-char -1))

         ;; Delete surrounding whitespace beyond a certain length.
         ;;
         ;; foo    |bar      -> foo |bar
         ;; foo    |    bar  -> foo | bar
         ((string-match-p (rx (+ space) eos) line-before-pt)
          (let ((has-space? (eq (char-after) ? )))
            (skip-chars-forward " ")
            (while (looking-back (rx space space) (line-beginning-position))
              (delete-char -1))
            (when has-space?
              (insert " ")
              (forward-char -1))))

         (t
          (funcall f args)))))))

;; Remove broken python indentation hack

(use-package smartparens-python
  :config
  (advice-remove 'python-indent-dedent-line-backspace
                 'ad-Advice-python-indent-dedent-line-backspace))
(provide 'cb-smartparens)

;;; cb-smartparens.el ends here
