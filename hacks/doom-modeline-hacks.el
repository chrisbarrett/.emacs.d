;;; doom-modeline-hacks.el --- <enter description here>  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)
(el-patch-feature doom-modeline-core)

(cl-eval-when (compile)
  (require 'fancy-battery)
  (require 'doom-modeline))

(with-eval-after-load 'doom-modeline-core
  (el-patch-defun doom-modeline-set-modeline (key &optional default)
    "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
  If DEFAULT is non-nil, set the default mode-line for all buffers."
    (when-let ((modeline (doom-modeline key)))
      (el-patch-wrap 2
        (unless (frame-parent)
          (setf (if default
                    (default-value (el-patch-swap 'mode-line-format 'header-line-format))
                  (buffer-local-value (el-patch-swap 'mode-line-format 'header-line-format) (current-buffer)))
                (list "%e" modeline)))))))

(provide 'doom-modeline-hacks)

;;; doom-modeline-hacks.el ends here
