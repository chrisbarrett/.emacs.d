;;; eshell-hacks.el --- Patches for eshell  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature eshell)



;; Write aliases in alphabetical order to make version control tidier.

(with-eval-after-load 'eshell
  (with-no-warnings
    (el-patch-defun eshell/alias (&optional alias &rest definition)
      "Define an ALIAS in the user's alias list using DEFINITION."
      (if (not alias)
          (dolist (alias (el-patch-swap eshell-command-aliases-list
                                        (sort eshell-command-aliases-list
                                              (lambda (l r)
                                                (string< (car l) (car r))))))
            (eshell-print (apply 'format "alias %s %s\n" alias)))
        (if (not definition)
            (setq eshell-command-aliases-list
                  (delq (assoc alias eshell-command-aliases-list)
                        eshell-command-aliases-list))
          (and (stringp definition)
               (set-text-properties 0 (length definition) nil definition))
          (let ((def (assoc alias eshell-command-aliases-list))
                (alias-def (list alias
                                 (eshell-flatten-and-stringify definition))))
            (if def
                (setq eshell-command-aliases-list
                      (delq def eshell-command-aliases-list)))
            (setq eshell-command-aliases-list
                  (cons alias-def eshell-command-aliases-list))))
        (eshell-write-aliases-list))
      nil)))

(provide 'eshell-hacks)

;;; eshell-hacks.el ends here
