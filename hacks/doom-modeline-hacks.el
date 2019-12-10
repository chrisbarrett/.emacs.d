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

(with-eval-after-load 'doom-modeline-segments
  (el-patch-defun doom-modeline-update-battery-status ()
    "Update battery status."
    (setq doom-modeline--battery-status
          (when (bound-and-true-p display-battery-mode)
            (let* ((data (and (bound-and-true-p battery-status-function)
                              (funcall battery-status-function)))
                   (charging?  (string-equal "AC" (cdr (assoc ?L data))))
                   (percentage (el-patch-wrap 1 (ignore-errors (min (car (read-from-string (cdr (assq ?p data))))
                                                                    battery-mode-line-limit))))
                   (face (when (numberp percentage)
                           (cond (charging? 'success)
                                 ((< percentage battery-load-critical) 'error)
                                 ((< percentage 25) 'warning)
                                 ((< percentage 95) 'mode-line)
                                 (t 'success))))
                   (icon (if (numberp percentage)
                             (cond (charging?
                                    (doom-modeline-icon 'alltheicon
                                                        "battery-charging"
                                                        "🔋"
                                                        "+"
                                                        face
                                                        :height 1.4
                                                        :v-adjust -0.1))
                                   ((> percentage 95)
                                    (doom-modeline-icon 'faicon
                                                        "battery-full"
                                                        "🔋"
                                                        "-"
                                                        face
                                                        :v-adjust -0.0575))
                                   ((> percentage 70)
                                    (doom-modeline-icon 'faicon
                                                        "battery-three-quarters"
                                                        "🔋"
                                                        "-"
                                                        face
                                                        :v-adjust -0.0575))
                                   ((> percentage 40)
                                    (doom-modeline-icon 'faicon
                                                        "battery-half"
                                                        "🔋"
                                                        "-"
                                                        face
                                                        :v-adjust -0.0575))
                                   ((> percentage battery-load-critical)
                                    (doom-modeline-icon 'faicon
                                                        "battery-quarter"
                                                        "🔋"
                                                        "-"
                                                        face
                                                        :v-adjust -0.0575))
                                   (t (doom-modeline-icon 'faicon
                                                          "battery-empty"
                                                          "🔋"
                                                          "!"
                                                          face
                                                          :v-adjust -0.0575)))
                           (doom-modeline-icon 'faicon
                                               "battery-empty"
                                               "⚠"
                                               "N/A"
                                               'error
                                               :v-adjust -0.0575)))
                   (text (if (numberp percentage)
                             (format "%d%%%%" percentage)
                           ""))
                   (help-echo (if (and battery-echo-area-format (numberp percentage))
                                  (battery-format battery-echo-area-format data)
                                "Battery status not available")))
              (concat (doom-modeline-spc)
                      (propertize (concat icon
                                          (doom-modeline-vspc)
                                          (propertize text 'face face))
                                  'help-echo help-echo)
                      (doom-modeline-spc)))))))

(provide 'doom-modeline-hacks)

;;; doom-modeline-hacks.el ends here
