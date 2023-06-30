;;; cb-startup-profiling-and-debugging.el --- Profile slow startups  -*- lexical-binding: t; -*-

;;; Commentary:

;; Hook into the startup lifecycle to report useful debugging information. Note
;; that these leverage the `DEPTH' argument so that subsequent functions added to
;; `after-init-hook' are still ordered correctly.

;;; Code:

;; Show profiler report if startup took too long

(require 'profiler)

(defcustom cb-startup-profiling-enabled-p t
  "Whether to show the debugger if startup takes to long.

This can be disabled in the site file."
  :type 'boolean
  :group 'cb-profiling-and-debugging)


(defcustom cb-startup-profiling-threshold-seconds 1
  "The maximum time startup can take before the profiler is shown."
  :group 'cb-profiling-and-debugging
  :type 'number)



(defun config-start-debugger-on-startup ()
  (when cb-startup-profiling-enabled-p
    (profiler-start 'cpu)))

(defun config-stop-debugger-on-startup ()
  (when (and cb-startup-profiling-enabled-p (boundp 'emacs-start-time))
    (profiler-stop)
    (let* ((now (current-time))
           (total-startup-time (float-time (time-subtract now emacs-start-time))))
      (unless (time-less-p total-startup-time
                           (seconds-to-time cb-startup-profiling-threshold-seconds))
        (profiler-report)))))

(unless noninteractive
  (add-hook 'after-init-hook #'config-start-debugger-on-startup -99)
  (add-hook 'after-init-hook #'config-stop-debugger-on-startup 99))

;; Enable debugging for duration of startup sequence.

(unless noninteractive
  (setq debug-on-error t))

(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil))
          98)

;; Log startup time duration.

(defvar cb-after-init-start-time)
(defvar cb-total-startup-duration)

(add-hook 'after-init-hook (lambda ()
                             (setq cb-after-init-start-time (current-time)))
          -97)

(add-hook 'after-init-hook (lambda ()
                             (when (boundp 'emacs-start-time)
                               (let* ((now (current-time))
                                      (after-init-duration (float-time (time-subtract now cb-after-init-start-time))))
                                 (setq cb-total-startup-duration (float-time (time-subtract now emacs-start-time)))
                                 (message "after-init completed (%.3f hook duration, %.3f seconds total startup time)"
                                          after-init-duration
                                          cb-total-startup-duration))))
          97)

(provide 'cb-startup-profiling-and-debugging)

;;; cb-startup-profiling-and-debugging.el ends here
