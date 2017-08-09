;;; cb-smartparens.el --- Smartparens config.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'dash)
  (require 'cb-use-package-extensions))

(require 'spacemacs-keys)
(require 'evil)

(use-package smartparens
  :defer t

  :functions (sp-local-pair
              sp-pair
              sp-get-pair
              sp--get-opening-regexp
              sp--get-closing-regexp
              sp-get-enclosing-sexp)
  :commands (smartparens-mode
             sp-up-sexp
             sp-kill-hybrid-sexp
             smartparens-strict-mode
             smartparens-global-strict-mode
             show-smartparens-global-mode)

  :preface
  (progn
    (autoload 'thing-at-point-looking-at "thingatpt")
    (autoload 's-matches? "s")

    (defun cb-smartparens--this-command-is-eval-expression (&rest _)
      (equal this-command 'eval-expression))

    (defun cb-smartparens--org-skip-asterisk (_ mb me)
      (or (and (= (line-beginning-position) mb)
               (eq 32 (char-after (1+ mb))))
          (and (= (1+ (line-beginning-position)) me)
               (eq 32 (char-after me)))))

    (defun cb-smartparens--sp-for-eval-expression ()
      (when (eq this-command 'eval-expression)
        (smartparens-mode)))

    (defun cb-smartparens-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (sp-get-pair id :cl-l))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

    (defun cb-smartparens-web-mode-is-code-context (_id action _context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (defun cb-smartparens--web-mode-format-paren-after-keyword (_id action context)
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

    (defun cb-smartparens-delete-horizontal-space-for-delete (f &rest args)
      (-if-let ((&plist :beg beg :end end :op op :cl cl) (sp-get-enclosing-sexp))
          (let* ((inside-start (+ beg (length op)))
                 (inside-end (- end (length cl)))
                 (before (buffer-substring inside-start (point)))
                 (after (buffer-substring (point) inside-end))
                 (inside (concat before after)))
            (cond
             ;; Delete contents for multiline pairs that were just inserted, e.g. braces.
             ((string-match-p (rx bos (* space) "\n" (* space) "\n" (* space) eos) inside)
              (delete-region inside-start inside-end))

             ;; Delete preceding whitespace beyond a certain length.
             ((let ((preceding (buffer-substring (line-beginning-position) (point))))
                (string-match-p (rx space (+ space) eos) preceding))
              (while (looking-back (rx space space) (line-beginning-position))
                (delete-char -1)))

             ;; Collapse horizontal space in empty pairs.
             ((string-match-p (rx bos (+ space) eos) inside)
              (delete-region inside-start inside-end))

             (t
              (funcall f args))))

        ;; Delete preceding whitespace beyond a certain length.
        (if (let ((preceding (buffer-substring (line-beginning-position) (point))))
              (string-match-p (rx space (+ space) eos) preceding))
            (while (looking-back (rx space space) (line-beginning-position))
              (delete-char -1))
          (funcall f args))))

    (defun cb-smartparens-add-space-before-sexp-insertion (id action _context)
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

  :leader-bind
  ((",A" . sp-add-to-previous-sexp)
   (",a" . sp-add-to-next-sexp)
   (",B" . sp-backward-barf-sexp)
   (",b" . sp-forward-barf-sexp)
   (",M" . sp-backward-slurp-sexp)
   (",m" . sp-forward-slurp-sexp)
   (",c" . sp-convolute-sexp)
   (",D" . sp-backward-kill-sexp)
   (",d" . sp-kill-sexp)
   (",e" . sp-emit-sexp)
   (",l" . sp-end-of-sexp)
   (",h" . sp-beginning-of-sexp)
   (",j" . sp-join-sexp)
   (",K" . sp-splice-sexp-killing-backward)
   (",k" . sp-splice-sexp-killing-forward)
   (",n" . sp-next-sexp)
   (",p" . sp-previous-sexp)
   (",r" . sp-raise-sexp)
   (",s" . sp-splice-sexp-killing-around)
   (",t" . sp-transpose-sexp)
   (",U" . sp-backward-unwrap-sexp)
   (",u" . sp-unwrap-sexp)
   (",w" . sp-rewrap-sexp)
   (",x" . sp-split-sexp)
   (",Y" . sp-backward-copy-sexp)
   (",y" . sp-copy-sexp)
   (",," . sp-previous-sexp)
   (",." . sp-next-sexp)
   (",<" . sp-backward-down-sexp)
   (",>" . sp-down-sexp))

  :init
  (progn
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'text-mode-hook #'smartparens-strict-mode)
    (add-hook 'minibuffer-setup-hook #'cb-smartparens--sp-for-eval-expression))

  :config
  (progn
    (setq sp-show-pair-delay 0.2)
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-highlight-pair-overlay nil)
    (setq sp-highlight-wrap-overlay nil)
    (setq sp-highlight-wrap-tag-overlay nil)
    (setq sp-navigate-close-if-unbalanced t)
    (setq sp-message-width nil)

    (require 'smartparens-config)
    (require 'smartparens-scala)
    (require 'smartparens-rust)

    (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

    (with-eval-after-load 'evil
      (define-key (with-no-warnings evil-insert-state-map) ")" #'sp-up-sexp))

    ;; Configure global pairs.

    (sp-pair "`" "`"
             :bind "M-`")
    (sp-pair "{" "}"
             :bind "M-{"
             :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "[" "]"
             :bind "M-["
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "(" ")"
             :bind "M-("
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    (sp-pair "\"" "\""
             :bind "M-\""
             :pre-handlers '(:add (cb-smartparens-add-space-before-sexp-insertion)))

    ;; Configure local pairs.

    (sp-with-modes 'minibuffer-inactive-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "(" nil
                     :when (cb-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :when (cb-smartparens--this-command-is-eval-expression)
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes sp-lisp-modes
      (sp-local-pair "(" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "\"" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "{" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion)))

    (sp-with-modes 'haskell-mode
      (sp-local-pair "(" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))
      (sp-local-pair "[" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))

      (sp-local-pair "{" nil
                     :pre-handlers '(cb-smartparens-add-space-before-sexp-insertion)
                     :post-handlers '(cb-smartparens-add-space-after-sexp-insertion))

      (sp-local-pair "[|" "|]" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "{-" "-}" :post-handlers '(("||\n[i]" "RET") ("- " "SPC")))
      (sp-local-pair "{-#" "#-}" :post-handlers '(("||\n[i]" "RET") ("-# " "SPC"))))

    (sp-with-modes 'web-mode
      (sp-local-pair "<" nil :when '(cb-smartparens-web-mode-is-code-context)))

    (sp-with-modes 'cb-web-js-mode
      ;; Flow strict types
      (sp-local-pair "{|" "|}" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "(" ")" :pre-handlers '(:add cb-smartparens--web-mode-format-paren-after-keyword)))

    (sp-with-modes 'org-mode
      (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'cb-smartparens--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
      (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»"))

    ;; Delete enclosing whitespace if necessary.

    (advice-add 'sp-backward-delete-char :around #'cb-smartparens-delete-horizontal-space-for-delete)

    ;; Enable modes.

    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1)))

(use-package which-key
  :config
  (progn
    (push `((nil . "sp-splice-sexp-killing-around") . (nil . "splice-killing-around"))
          which-key-replacement-alist)
    (push `((nil . "sp-splice-sexp-killing-forward") . (nil . "splice-killing-forward"))
          which-key-replacement-alist)
    (push `((nil . "sp-splice-sexp-killing-backward") . (nil . "splice-killing-backward"))
          which-key-replacement-alist)

    (push `((nil . ,(rx bos "sp-" (group (+? nonl))
                        "-sexp" eos)) . (nil . "\\1"))
          which-key-replacement-alist)))

(provide 'cb-smartparens)

;;; cb-smartparens.el ends here
