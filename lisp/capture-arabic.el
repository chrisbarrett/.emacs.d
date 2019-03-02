;;; capture-arabic.el --- Supporting functions for org-capture  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)



(defvar capture-arabic--arabic-input-history nil)

(defvar capture-arabic--english-input-history nil)

(defun capture-arabic--read-en (prompt &optional default)
  (read-string (concat "[en] " prompt) default 'capture-arabic--english-input-history))

(defun capture-arabic--read-ar (prompt)
  (let ((setup (lambda () (set-input-method "walrus-arabic"))))
    (add-hook 'minibuffer-setup-hook setup)
    (unwind-protect
        (read-string (concat "‎[العربية] " prompt) nil 'capture-arabic--arabic-input-history nil t)
      (remove-hook 'minibuffer-setup-hook setup))))

(defun capture-arabic--read-gender ()
  (char-to-string
   (read-char-choice "Choose gender: \n    [m]ale\n    [f]emale"
                     (list ?m ?f))))



(defun capture-arabic--pluralise-en (word)
  (cond
   ((string-suffix-p "us" word)
    (concat (string-remove-suffix "us" word) "i"))
   ((string-suffix-p "on" word)
    (concat (string-remove-suffix "on" word) "a"))
   ((or (string-suffix-p "s" word)
        (string-suffix-p "o" word)
        (string-suffix-p "sh" word)
        (string-suffix-p "ch" word)
        (string-suffix-p "is" word)
        (string-suffix-p "x" word)
        (string-suffix-p "z" word))
    (concat word "es"))
   (t
    (concat word "s"))))


(defun capture-arabic-read-noun-as-table-row ()
  (let ((en-sing (capture-arabic--read-en "Singular: ")))
    (concat "|"  en-sing
            "|" (capture-arabic--read-en "plural: " (capture-arabic--pluralise-en en-sing))
            "|" (capture-arabic--read-ar "singular: ")
            "|" (capture-arabic--read-ar "plural: ")
            "|" (capture-arabic--read-gender)
            "|")))

(provide 'capture-arabic)

;;; capture-arabic.el ends here
