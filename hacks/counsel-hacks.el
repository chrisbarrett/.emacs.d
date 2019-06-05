;;; counsel-hacks.el --- Patches for counsel  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash-functional)
(require 'el-patch)
(require 'f)
(require 'subr-x)

(el-patch-feature counsel)



;; Show abbreviated filepaths in recentf list.

(with-eval-after-load 'counsel
  (with-no-warnings
    (defun counsel-hacks-recentf-candidates ()
      (seq-uniq (seq-map (-compose #'f-abbrev #'substring-no-properties #'file-truename)
                         recentf-list)))

    (el-patch-defun counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: " (el-patch-swap
                              (mapcar #'substring-no-properties recentf-list)
                              (counsel-hacks-recentf-candidates))
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))))



;; Prefill counsel search functions with the symbol at point.

(autoload 'ivy-immediate-done "ivy")

(defun counsel-hacks--populate-with-symbol-at-point (f &rest args)
  (if-let* ((sym (symbol-at-point)))
      (apply f (symbol-name sym) (cdr args))
    (apply f args)))

(advice-add 'counsel-rg :around #'counsel-hacks--populate-with-symbol-at-point)
(advice-add 'counsel-ag :around #'counsel-hacks--populate-with-symbol-at-point)

(provide 'counsel-hacks)

;;; counsel-hacks.el ends here
