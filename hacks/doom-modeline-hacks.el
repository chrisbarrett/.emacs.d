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
  (el-patch-defun doom-modeline-update-battery-status (&optional status)
    "Update battery STATUS."
    (setq doom-modeline--battery-status
          (let* ((status (or status fancy-battery-last-status))
                 (charging?  (string-equal "AC" (cdr (assoc ?L status))))
                 (percentage (cdr (assq ?p status)))
                 (percentage-number (string-to-number percentage))
                 (face (cond
                        (charging? 'fancy-battery-charging)
                        ((< percentage-number 10) 'fancy-battery-critical)
                        ((< percentage-number 25) 'fancy-battery-discharging)
                        ((< percentage-number 95) 'mode-line)
                        (t 'fancy-battery-charging)))
                 (icon (cond
                        (charging?
                         (if doom-modeline-icon
                             (doom-modeline-icon-alltheicon "battery-charging"
                                                            :face face
                                                            :height 1.4
                                                            :v-adjust -0.1)
                           "+"))
                        ((> percentage-number 95)
                         (if doom-modeline-icon
                             (doom-modeline-icon-faicon "battery-full"
                                                        :face face
                                                        :v-adjust -0.0575)
                           "-"))
                        ((> percentage-number 70)
                         (if doom-modeline-icon
                             (doom-modeline-icon-faicon "battery-three-quarters"
                                                        :face face
                                                        :v-adjust -0.0575)
                           "-"))
                        ((> percentage-number 40)
                         (if doom-modeline-icon
                             (doom-modeline-icon-faicon "battery-half"
                                                        :face face
                                                        :v-adjust -0.0575)
                           "-"))
                        ((> percentage-number 15)
                         (if doom-modeline-icon
                             (doom-modeline-icon-faicon "battery-quarter"
                                                        :face face
                                                        :v-adjust -0.0575)
                           "-"))
                        (t
                         (if doom-modeline-icon
                             (doom-modeline-icon-faicon "battery-empty"
                                                        :face face
                                                        :v-adjust -0.0575)
                           "!"))))
                 (percent-str (and percentage (concat percentage "%%")))
                 (help-echo (if battery-echo-area-format
                                (battery-format battery-echo-area-format status)
                              "Battery status not available")))
            (concat
             (doom-modeline-spc)
             (if percent-str
                 (concat
                  (propertize icon 'help-echo help-echo)
                  (if doom-modeline-icon (doom-modeline-vspc))
                  (el-patch-wrap 2 (when (bound-and-true-p fancy-battery-show-percentage)
                                     (propertize percent-str 'face face 'help-echo help-echo))))
               ;; Battery status is not available
               (if doom-modeline-icon
                   (doom-modeline-icon-faicon "battery-empty" :v-adjust -0.0575 :face 'error)
                 (propertize "N/A"
                             'face 'error
                             'help-echo "Battery status not available")))
             (doom-modeline-spc))))))

(provide 'doom-modeline-hacks)

;;; doom-modeline-hacks.el ends here
