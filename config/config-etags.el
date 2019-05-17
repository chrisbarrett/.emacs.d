;;; config-etags.el --- Configuration for etags  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar config-etags-in-query-replace-session-p nil
  "Indicates whether a tags loop command is currently in progress.")

(use-package etags
  :preface
  (progn
    (autoload 'tags-loop-continue "etags")

    ;; Redeclare var from etags.el
    (defvar tags-loop-operate nil)

    (defun config-etags--save-and-cleanup-buffers (f &rest args)
      (unwind-protect
          (let* ((existing-buffers (buffer-list))
                 ;; Wrap the original tags-loop program cleanup logic.
                 (tags-loop-operate
                  `(let ((continue-p ,tags-loop-operate))
                     (save-buffer)
                     (unless (seq-contains (list ,@existing-buffers) (current-buffer))
                       (kill-buffer))
                     (setq config-etags-in-query-replace-session-p (not (null continue-p)))
                     continue-p)))
            (apply f args))
        (setq config-etags-in-query-replace-session-p nil))))

  :config
  (advice-add 'tags-loop-continue :around #'config-etags--save-and-cleanup-buffers))


(provide 'config-etags)

;;; config-etags.el ends here
