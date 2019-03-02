;;; capture-arabic.el --- Supporting functions for org-capture  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'subr-x)



(defvar capture-arabic--arabic-input-history nil)

(defvar capture-arabic--english-input-history nil)

(defun capture-arabic--read-en (prompt &optional default)
  (let ((prompt-str (concat "‎[en] "
                            (if default
                                (concat prompt " [default: " default "]")
                              prompt)
                            ": ")))
    (read-string prompt-str nil 'capture-arabic--english-input-history default)))

(defun capture-arabic--read-ar (prompt &optional default)
  (let ((setup (lambda () (set-input-method "walrus-arabic")))
        (prompt-str (concat "‎[العربية] "
                            (if default
                                (concat prompt " [default: " default "]")
                              prompt)
                            ": ")))
    (add-hook 'minibuffer-setup-hook setup)
    (unwind-protect
        (read-string prompt-str nil 'capture-arabic--arabic-input-history default t)
      (remove-hook 'minibuffer-setup-hook setup))))

(defun capture-arabic--read-gender ()
  (char-to-string
   (read-char-choice "Choose gender: \n    [m]ale\n    [f]emale"
                     (list ?m ?f))))



(defun capture-arabic--pluralise-en (word-or-words)
  (cl-labels ((pluralise
               (word)
               (cond
                ((string-suffix-p "us" word)
                 (concat (string-remove-suffix "us" word) "i"))
                ((string-suffix-p "on" word)
                 (concat (string-remove-suffix "on" word) "a"))
                ((string-match-p (rx (not (any "yaeiou")) "y" eos)
                                 word)
                 (concat (string-remove-suffix "y" word) "ies"))
                ((or (string-suffix-p "s" word)
                     (string-suffix-p "o" word)
                     (string-suffix-p "sh" word)
                     (string-suffix-p "ch" word)
                     (string-suffix-p "is" word)
                     (string-suffix-p "x" word)
                     (string-suffix-p "z" word))
                 (concat word "es"))
                (t
                 (concat word "s")))))
    (let ((words (split-string word-or-words (rx ",")
                               t
                               (rx (+ space)))))
      (string-join (-map #'pluralise words) ", "))))

(defun capture-arabic--pluralise-ar (word)
  (cond
   ((string-suffix-p "ة" word)
    (concat (string-remove-suffix "ة" word) "ات"))))

(defun capture-arabic-read-noun-as-table-row ()
  (let* ((en-sing (capture-arabic--read-en "singular"))
         (en-pl (capture-arabic--read-en "plural" (capture-arabic--pluralise-en en-sing)))
         (ar-sing (capture-arabic--read-ar "singular")))
    (concat "|" en-sing
            "|" en-pl
            "|" ar-sing
            "|" (capture-arabic--read-ar "plural" (capture-arabic--pluralise-ar ar-sing))
            "|" (capture-arabic--read-gender)
            "|"
            "|")))

(defun capture-arabic-read-phrase-as-table-row ()
  (concat "|" (capture-arabic--read-en "phrase")
          "|" (capture-arabic--read-ar "phrase")
          "|"))

(defun capture-arabic-read-verb-as-table-row ()
  (concat "|" (capture-arabic--read-en "verb")
          "|" (capture-arabic--read-ar "past")
          "|" (capture-arabic--read-ar "present")
          "|" (capture-arabic--read-ar "masdar")
          "|"))

(provide 'capture-arabic)

;;; capture-arabic.el ends here
