;;; capture-arabic.el --- Supporting functions for org-capture  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

(defvar capture-arabic--arabic-input-history nil)

(defvar capture-arabic--english-input-history nil)



(defun capture-arabic--read-english (prompt)
  (read-string prompt nil 'capture-arabic--english-input-history))

(defun capture-arabic--read-arabic (prompt)
  (let ((setup (lambda () (set-input-method "walrus-arabic"))))
    (add-hook 'minibuffer-setup-hook setup)
    (unwind-protect
        (read-string prompt nil 'capture-arabic--arabic-input-history nil t)
      (remove-hook 'minibuffer-setup-hook setup))))



(defun capture-arabic-read-noun ()
  (let ((english (capture-arabic--read-english "English word: "))
        (arabic-singular (capture-arabic--read-arabic "Arabic Singular: "))
        (arabic-plural (capture-arabic--read-arabic "Arabic Plural: "))
        (properties
         (concat ":PROPERTIES:\n"
                 ":ID: " (org-id-uuid) "\n"
                 ":DRILL_CARD_TYPE: multisided\n"
                 ":END:")))
    (string-join (-non-nil (list
                            (format "* %s: %s        :drill:\n%s" english
                                    (with-temp-buffer
                                      (insert arabic-singular)
                                      (unless (string-blank-p arabic-plural)
                                        (insert "،")
                                        (insert " ")
                                        (insert arabic-plural))
                                      (buffer-string))
                                    properties)
                            (format "** Definition      :noexport:\n%s" english)
                            (unless (string-blank-p arabic-singular)
                              (format "** Arabic Singular :noexport:\n%s" arabic-singular))
                            (unless (string-blank-p arabic-plural)
                              (format "** Arabic Plural   :noexport:\n%s" arabic-plural))))
                 "\n\n")))

(provide 'capture-arabic)

;;; capture-arabic.el ends here
