;;; org-templates.el --- Declarative org capture templates.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Provide a declarative syntax for declaring org-capture templates using files.
;; Each template file should begin with at least one lisp-data src block,
;; describing the metadata for the template.

;;; Code:

(require 'dash)
(require 'ht)
(require 'interpolate)
(require 'org-ml)
(require 'seq)

(defun org-templates--to-capture-template (spec)
  (-let* (((&plist :keys :description :type :target :template :params :options) spec)
          (fn-name (intern (format "org-templates--make-template-%s" keys))))

    (defalias fn-name (lambda ()
                        (apply #'interpolate template (when params (funcall params)))))

    (append (list keys description type target (list 'function fn-name))
            options)))

(defun org-templates--src-block-plist (path src-node)
  (let ((result (read (org-ml-get-property :value src-node))))

    (let ((required '(:keys :description :target)))
      (when-let* ((missing (seq-difference required (ht-keys (ht-from-plist result)))))
        (error "Error in %s: missing required key(s) %s" path missing)))

    (if-let* ((type (plist-get result :type)))
        (unless (symbolp type)
          (error "Error in %s: type must be a symbol but got value %s" path type))
      (plist-put result :type 'entry))

    (when-let* ((params (plist-get result :params)))
      (unless (or (functionp params) (listp params))
        (error "Error in %s: params must be a function or plist" path))

      ;; Normalise params to be a function if present.
      (when (not (functionp params))
        (plist-put result :params (lambda () (eval params)))))

    result))

(defun org-templates--parse (path nodes)
  (-let* ((parsed (org-ml-get-children nodes))
          (match-src-block '(:and src-block (:language "lisp-data")))
          (initial-src-blocks (org-ml-match `(:any * ,match-src-block) (car parsed)))
          (template (cons (org-ml-match-delete `(:any * ,match-src-block)
                                               (car parsed))
                          (cdr parsed)))
          (template-string (substring-no-properties (string-trim-left (string-join (seq-map #'org-ml-to-trimmed-string template) "\n")))))
    (seq-map (lambda (src-block)
               `(:template ,template-string ,@(org-templates--src-block-plist path src-block)))
             initial-src-blocks)))

;;;###autoload
(defun org-templates-from-buffer (buffer)
  "Return a list of capture templates described by an org template.

BUFFER is the buffer containing the org template to parse."
  (with-current-buffer buffer
    (seq-map #'org-templates--to-capture-template (org-templates--parse (buffer-name) (org-ml-parse-this-buffer)))))

;;;###autoload
(defun org-templates-from-file (path)
  "Return a list of capture templates described by an org template.

PATH is the path to an org template file to parse."
  (with-temp-buffer
    (insert-file-contents path)
    (seq-map #'org-templates--to-capture-template (org-templates--parse path (org-ml-parse-this-buffer)))))

(provide 'org-templates)

;;; org-templates.el ends here
