;;; cb-gc-tuning.el --- Runtime garbage-collection tuning  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom cb-gc-normal-threshold 100000000
  "The gc threshold to use during normal operation."
  :type 'number
  :group 'cb-gc-tuning)



;; GCs can cause a noticeable disruptive pause when using interactive commands.
;; Prevent collections while the minibuffer is active for user input .

(defun cb-gc-inhibit ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun cb-gc-normal ()
  (setq gc-cons-threshold cb-gc-normal-threshold))

;; Restore GC settings after init sequence has completed

(add-hook 'after-init-hook #'cb-gc-normal)

;; Prevent GCs during user input in the minibuffer

(add-hook 'minibuffer-setup-hook #'cb-gc-inhibit)
(add-hook 'minibuffer-exit-hook #'cb-gc-normal)

(provide 'cb-gc-tuning)

;;; cb-gc-tuning.el ends here
