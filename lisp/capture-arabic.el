;;; capture-arabic.el --- Supporting functions for org-capture  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
        (arabic-plural (capture-arabic--read-arabic "Arabic Plural: ")))
    (format "- %s :: %s" english
            (with-temp-buffer
              (insert arabic-singular)
              (insert "،")
              (insert " ")
              (insert arabic-plural)
              (buffer-string)))))

(provide 'capture-arabic)

;;; capture-arabic.el ends here
