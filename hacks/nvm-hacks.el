;;; nvm-hacks.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature nvm)



;; Unfortunately, nvm doesn't actually put the node bin into the exec path. Fix
;; that here.

(with-eval-after-load 'nvm
  (with-no-warnings
    (el-patch-defun nvm-use (version &optional callback)
      "Activate Node VERSION.

If CALLBACK is specified, active in that scope and then reset to
previously used version."
      (setq version (nvm--find-exact-version-for version))
      (let ((version-path (-last-item version)))
        (if (nvm--version-installed? (car version))
            (let ((prev-version nvm-current-version)
                  (prev-exec-path exec-path))
              (setenv "NVM_BIN" (f-join version-path "bin"))
              (setenv "NVM_PATH" (f-join version-path "lib" "node"))
              (let* ((path-re (concat "^" (f-join nvm-dir nvm-runtime-re) nvm-version-re "/bin/?$"))
                     (new-bin-path (f-full (f-join version-path "bin")))
                     (el-patch-remove
                       (paths
                        (cons
                         new-bin-path
                         (-reject
                          (lambda (path)
                            (s-matches? path-re path))
                          (parse-colon-path (getenv "PATH")))))))
                (el-patch-remove (setenv "PATH" (s-join path-separator paths)))
                (setq exec-path (cons new-bin-path (--remove (s-matches? path-re it) exec-path)))
                (el-patch-add (setenv "PATH" (s-join path-separator (--map (s-chop-suffix "/" it) exec-path)))))
              (setq nvm-current-version version)
              (when callback
                (unwind-protect
                    (funcall callback)
                  (when prev-version (nvm-use (car prev-version)))
                  (setq exec-path prev-exec-path))))
          (error "No such version %s" version))))))

(provide 'nvm-hacks)

;;; nvm-hacks.el ends here
