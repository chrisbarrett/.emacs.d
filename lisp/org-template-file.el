;;; org-template-file.el --- Declarative file templates  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'ht)
(require 'interpolate)
(require 'om)
(require 'seq)

(defun org-template-file--to-capture-template (spec)
  (-let* (((&plist :keys :description :type :target :template :params :options) spec)
          (make-template (list 'function
                               (lambda ()
                                 (apply #'interpolate template (funcall params))))))
    (append (list keys description type target make-template)
            options)))

(defun org-template-file--src-block-plist (src-node)
  (let (result)
    (om-match-do '(:any) (lambda (it)
                           (setq result (read (om-get-property :value it))))
                 src-node)

    (let ((required '(:keys :description :target)))
      (when-let* ((missing (seq-difference required (ht-keys (ht-from-plist result)))))
        (error "Template missing required key(s): %s" missing)))

    (if-let* ((type (plist-get result :type)))
        (unless (symbolp type)
          (error ":type must be a symbol"))
      (plist-put result :type 'entry))

    (when-let* ((params (plist-get result :params)))
      (unless (or (functionp params) (listp params))
        (error ":params must be a function or plist"))

      ;; Normalise params to be a function.
      (when (not (functionp params))
        (plist-put result :params (lambda () (eval params)))))

    result))

(defun org-template-file--parse (str)
  (-let* ((parsed (with-temp-buffer
                    (insert str)
                    (goto-char (point-min))
                    (om-get-children (om-parse-this-buffer))))
          (match-arg-src-block '(:and src-block (:language "emacs-lisp")))
          (arg-src-block (om-match (list :first match-arg-src-block) (car parsed)))
          (template (cons (om-match-delete `(:many ,match-arg-src-block)
                                           (car parsed))
                          (cdr parsed)))
          (template-string (string-trim-left (string-join (seq-map #'om-to-trimmed-string template) "\n"))))
    `(:template ,template-string ,@(org-template-file--src-block-plist arg-src-block))))

(defun org-template-file-from-string (str)
  (org-template-file--to-capture-template (org-template-file--parse str)))

(defun org-template-file (path)
  (org-template-file-from-string (f-read-text path)))

(provide 'org-template-file)

;;; org-template-file.el ends here
