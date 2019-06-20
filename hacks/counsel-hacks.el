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

(provide 'counsel-hacks)

;;; counsel-hacks.el ends here
