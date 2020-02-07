;;; autoinsert-files.el --- Fancy autoinsert files  -*- lexical-binding: t; -*-

;;; Commentary:

;; Use yasnippet to expand autoinsert templates, and use these files to
;; declaratively define the available templates.

;;; Code:

(require 'autoinsert)
(require 'f)
(require 's)
(require 'yasnippet)

(defun autoinsert-files--compile (form)
  (pcase form
    ((or (pred stringp) (pred symbolp))
     form)
    (`(rx . ,_)
     (eval form))
    (_
     (error "Unknown form in snippet: %s" form))))

(defun autoinsert-files--parse-header (file str)
  (condition-case _
      (list
       :name (cadr (s-match (rx bol (* space) "#" (* space) "name" (* space) ":" (* space) (group (+ nonl))) str))
       :pred (read (cadr (s-match (rx bol (* space) "#" (* space) "match" (* space) ":" (* space) (group (+ nonl))) str))))
    (error
     (error "Syntax error reading template %s" file))))

(defun autoinsert-files--parse-template (file str)
  (pcase (-map #'s-trim (s-split "# --" str))
    (`(,header ,snippet)
     (let ((plist (autoinsert-files--parse-header file header)))
       (append plist (list :snippet snippet))))
    (_
     (error "Malformed snippet %s" file))))

(defun autoinsert-files-populate-templates ()
  (let ((templates (--map (autoinsert-files--parse-template it (f-read-text it)) (f-entries auto-insert-directory))))

    (dolist (template templates)
      (-let* (((&plist :pred :name :snippet) template)
              (condition (autoinsert-files--compile pred)))
        (push `((,condition . ,name) . (lambda ()
                                         (insert ,snippet)
                                         (yas-expand-snippet (buffer-string) (point-min) (point-max))))
              auto-insert-alist)))

    (setq auto-insert-alist (seq-uniq auto-insert-alist
                                      (lambda (l r)
                                        (equal (cdar l) (cdar r)))))))

(provide 'autoinsert-files)

;;; autoinsert-files.el ends here
