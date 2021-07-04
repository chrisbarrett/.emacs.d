;;; interpolate.el --- String interpolation.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Taken from: https://gist.github.com/0branch/1240468/c8e533c454ef12b3469acbae669100ff6a0f7c23

;;; Code:

(require 'ht)
(require 's)

(defconst interpolate-regexp (rx "%%" (+ (any alnum "_" "-"))))

(defun interpolate--lookup (ht key &optional bindings-alist)
  (let* ((not-found (gensym))
         (value (ht-get ht key not-found)))
    (when (equal not-found value)
      (setq value (eval key bindings-alist))
      (puthash key value ht))
    value))

;;;###autoload
(defun interpolate-string (string &optional bindings-alist)
  "Interpolate STRING with variables from the global environment.

Values not bound in the environment can be provided with BINDINGS-ALIST."
  (with-temp-buffer
    (insert string)
    (let ((matches (s-matched-positions-all interpolate-regexp (buffer-string)))
          (values (ht-create)))
      (pcase-dolist (`(,beg . ,end) (nreverse matches))
        (let* ((var (intern (buffer-substring (+ 3 beg) (1+ end))))
               (value (interpolate--lookup values var bindings-alist)))
          (goto-char (1+ beg))
          (delete-region (1+ beg) (1+ end))
          (insert (format "%s" value))))
      (buffer-string))))

(defun interpolate--keyword-args-to-alist (kws-plist)
  (ht-map (lambda (k v)
            (let ((sym (intern (s-chop-prefix ":" (format "%s" k)))))
              (cons sym v)))
          (ht-from-plist kws-plist)))

;;;###autoload
(defun interpolate (string &rest keys)
  "Interpolate values in STRING using `%%value' syntax.

Values not bound in the global environment can be provided with KEYS, e.g.

  (interpolate \"foo %%bar\" :bar \"baz\")
  => \"foo baz\""
  (interpolate-string string (interpolate--keyword-args-to-alist keys)))

(provide 'interpolate)

;;; interpolate.el ends here
