;;; config-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 's)
(require 'thingatpt)

(use-package smartparens
  :straight t
  :hook ((prog-mode . smartparens-strict-mode)
         (eshell-mode . smartparens-strict-mode)
         (text-mode . smartparens-strict-mode))

  :general (:keymaps 'smartparens-strict-mode-map [remap c-electric-backspace] #'sp-backward-delete-char)
  :general (:states 'insert ")" #'sp-up-sexp)
  :general (:states 'normal "D" #'sp-kill-hybrid-sexp)

  :functions (sp-local-pair
              sp-pair
              sp-get-pair
              sp--get-opening-regexp
              sp--get-closing-regexp
              sp-get-enclosing-sexp)
  :commands (smartparens-mode
             smartparens-global-strict-mode
             show-smartparens-global-mode)

  :preface
  (progn
    (defun config-smartparens--this-command-is-eval-expression (&rest _)
      (equal this-command 'eval-expression))

    (defun config-smartparens--point-in-square-brackets-p (_id action _context)
      (when (eq action 'insert)
        (thing-at-point-looking-at (rx "[" (* (any "/" space))))))

    (defun config-smartparens--format-org-checkitem (_id action _context)
      (when (and (equal action 'insert)
                 (org-at-item-p))
        (atomic-change-group
          (just-one-space)
          (search-backward "[" (line-beginning-position))
          (just-one-space)
          (search-forward "]" (line-end-position))
          (just-one-space))))

    (defun config-smartparens--in-src-block-p (_id action _context)
      (when (eq action 'insert)
        (org-in-src-block-p)))

    (defun config-smartparens--org-skip-asterisk (_ mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    (defun config-smartparens--sp-for-eval-expression ()
      (when (eq this-command 'eval-expression)
        (smartparens-mode)))

    (defun config-smartparens-k&r-curlies (&rest _)
      (save-excursion
        (search-backward "{")
        (newline-and-indent))
      (save-excursion
        (newline-and-indent))
      (indent-according-to-mode))

    (defun config-smartparens-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun config-smartparens-web-mode-is-code-context (_id action _context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (defun config-smartparens--web-mode-format-paren-after-keyword (_id action context)
      "Insert a space after some keywords."
      (when (and (equal action 'insert)
                 (equal context 'code)
                 (thing-at-point-looking-at
                  (rx symbol-start (or "=" "for" "of" "in"
                                       "if" "else" "while"
                                       "return"
                                       "yield" "yield*")
                      (* space) "(")))
        (save-excursion
          (search-backward "(")
          (just-one-space))))

    (defun config-smartparens-delete-horizontal-space-for-delete (f &rest args)
      "Perform context-sensitive whitespace cleanups when deleting."
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
          (funcall f args)))))

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
            (just-one-space)))))))

  :init
  (add-hook 'minibuffer-setup-hook #'config-smartparens--sp-for-eval-expression)

  :config
  (progn
    (general-setq
     sp-show-pair-delay 0.2
     sp-show-pair-from-inside t
     sp-cancel-autoskip-on-backward-movement nil
     sp-highlight-pair-overlay nil
     sp-highlight-wrap-overlay nil
     sp-highlight-wrap-tag-overlay nil
     sp-navigate-close-if-unbalanced t
     sp-message-width nil)

    (require 'smartparens-config)
    (require 'smartparens-rust)

    ;; Configure global pairs.

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

    ;; Configure local pairs.

    (sp-with-modes 'minibuffer-inactive-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil
                     :when (config-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :when (config-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes (cons 'eshell-mode sp-lisp-modes)
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

    (sp-with-modes 'haskell-mode
      (sp-local-pair "(" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "{" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion))

      (sp-local-pair "[|" "|]" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "{-" "-}" :post-handlers '(("||\n[i]" "RET") ("- " "SPC")))
      (sp-local-pair "{-#" "#-}" :post-handlers '(("||\n[i]" "RET") ("-# " "SPC"))))

    (sp-with-modes 'csharp-mode
      (sp-local-pair "{" "}"
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '((config-smartparens-k&r-curlies "RET") ("| " "SPC"))))

    (sp-with-modes '(nix-mode nix-repl-mode)
      (sp-local-pair "(" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion ("||\n[i]" "RET") ("| " "SPC"))))

    (sp-with-modes 'js-mode
      (sp-local-pair "\"" "\"" :bind "M-\"")
      ;; Flow strict types
      (sp-local-pair "{|" "|}" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "(" ")" :pre-handlers '(:add config-smartparens--web-mode-format-paren-after-keyword)))

    (sp-with-modes 'fstar-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "{" nil :pre-handlers nil :post-handlers '(config-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "(" nil
                     :pre-handlers '(config-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(config-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes 'org-mode
      (sp-local-pair "[" "]" :post-handlers '(config-smartparens--format-org-checkitem))
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p config-smartparens--in-src-block-p) :wrap "C-*" :skip-match 'config-smartparens--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p config-smartparens--in-src-block-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p config-smartparens--point-in-square-brackets-p config-smartparens--in-src-block-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p config-smartparens--in-src-block-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p config-smartparens--in-src-block-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»" :unless '(config-smartparens--in-src-block-p)))

    ;; Delete enclosing whitespace if necessary.

    (advice-add 'sp-backward-delete-char :around #'config-smartparens-delete-horizontal-space-for-delete)

    ;; Enable modes.

    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1)))

(provide 'config-smartparens)

;;; config-smartparens.el ends here
