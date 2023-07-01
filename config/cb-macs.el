;;; cb-macs.el --- Macros used in config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'general)

(defmacro mode-leader-set-key (&rest args)
  (declare (indent defun))
  `(general-def ,@args ,@'(:keymaps 'override :states
                           '(normal motion visual)
                           :prefix ",")))

(general-unbind :states '(normal motion) ",")

(provide 'cb-macs)

;;; cb-macs.el ends here
