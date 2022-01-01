;;; flycheck-org-lint.el --- Add flycheck support to org files via org-lint  -*- lexical-binding: t; -*-
;;; Commentary:

;; Source: https://tecosaur.github.io/emacs-config/config.html#flycheck-with-org

;;; Code:

(require 'flycheck)

(defconst flycheck-org-lint-form
  (flycheck-prepare-emacs-lisp-form
    (require 'org)
    (require 'org-attach)
    (require 'ol)
    (let ((source (car command-line-args-left))
          (process-default-directory default-directory))
      (with-temp-buffer
        (insert-file-contents source 'visit)
        (setq buffer-file-name source)
        (setq default-directory process-default-directory)
        (delay-mode-hooks (org-mode))
        (setq delayed-mode-hooks nil)
        (pcase-dolist (`(,_ [,line ,level ,error]) (org-lint))
          (princ (format "%s: %s - %s" line level error))
          (terpri))))))

(defconst flycheck-org-lint-variables
  '(org-directory
    org-id-locations
    org-id-locations-file
    org-attach-id-dir
    org-attach-use-inheritance
    org-attach-id-to-path-function-list
    org-link-parameters)
  "Variables inherited by the org-lint subprocess.")

(defun flycheck-org-lint-variables-form ()
  (require 'org-attach)  ; Needed to make variables available
  `(progn
     ,@(seq-map (lambda (opt)
                  (let ((value (if (equal opt 'org-link-parameters)
                                   (seq-map (lambda (it) (seq-take it 1)) org-link-parameters)
                                 (symbol-value opt))))
                    `(setq-default ,opt ',value)))
                (seq-filter #'boundp flycheck-org-lint-variables))))

(flycheck-define-checker org-lint
  "Org buffer checker using `org-lint'."
  :command ("emacs" (eval flycheck-emacs-args)
            "--eval" (eval (flycheck-sexp-to-string `(add-to-list 'load-path ,(file-name-directory (locate-library "org")))))
            "--eval" (eval (flycheck-sexp-to-string (flycheck-org-lint-variables-form)))
            "--eval" (eval flycheck-org-lint-form)
            "--" source)
  :error-patterns
  ((warning line-start line ": low - " (message) line-end)
   (error line-start line ": " (+? word) " - " (message) line-end))
  :modes org-mode)

(provide 'flycheck-org-lint)

;;; flycheck-org-lint.el ends here
