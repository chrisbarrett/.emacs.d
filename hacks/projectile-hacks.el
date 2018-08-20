;;; projectile-hacks.el --- Hacks for projectile.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature projectile)


;; Fix issue with projectile-compile.
;;
;; Can remove after [1] is merged.
;;
;; [1]: https://github.com/bbatsov/projectile/pull/1269

(with-eval-after-load 'projectile
  (with-no-warnings
    (el-patch-defun projectile-default-generic-command (project-type command-type)
      "Generic retrieval of COMMAND-TYPEs default cmd-value for PROJECT-TYPE.

If found, checks if value is symbol or string. In case of symbol resolves
to function `funcall's. Return value of function MUST be string to be executed as command."
      (let ((command (plist-get (gethash project-type projectile-project-types) command-type)))
        (cond
         ((stringp command) command)
         ((functionp command)
          (if (fboundp command)
              (funcall (symbol-function command))))
         (el-patch-add
           ((and (not command) (eq command-type 'compilation-dir))
            nil))
         (t
          (user-error "The value for: %s in project-type: %s was neither a function nor a string." command-type project-type)))))))


(provide 'projectile-hacks)

;;; projectile-hacks.el ends here
