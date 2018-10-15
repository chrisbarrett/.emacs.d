;;; web-mode-submodes.el --- Major modes derived from web-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'seq)

;; Placate byte-compiler

(eval-when-compile
  (require 'web-mode))

(autoload 'web-mode "web-mode")
(autoload 'thing-at-point-looking-at "thingatpt")



(defvar web-js-base-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "/") #'web-mode-js-electric-slash)
    keymap))

;;;###autoload
(define-derived-mode web-js-base-mode web-mode "JS"
  "Derived mode for editing JavaScript files."
  (when (seq-contains '("" "html") web-mode-content-type)
    (setq-local web-mode-content-type "javascript")))

;;;###autoload
(define-derived-mode web-js-mode web-js-base-mode "JS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode web-js-snap-mode web-js-mode "JS"
  "Derived mode for editing Jest snapshot files.")

;;;###autoload
(define-derived-mode web-ts-mode web-js-base-mode "TS"
  "Derived mode for editing JavaScript files.")

;;;###autoload
(define-derived-mode web-json-mode web-mode "JSON"
  "Derived mode for editing JSON files."
  (setq-local web-mode-content-type "json"))

;;;###autoload
(define-derived-mode web-html-mode web-mode "HTML"
  "Derived mode for editing HTML files."
  (setq-local web-mode-content-type "html"))

;;;###autoload
(define-derived-mode web-css-mode web-mode "CSS"
  "Derived mode for editing CSS files."
  (setq-local web-mode-content-type "css"))

;;;###autoload
(define-derived-mode web-mustache-mode web-mode "Mustache"
  "Derived mode for editing mustache files.")

;;;###autoload
(define-derived-mode avro-mode web-mode "Avro"
  "Derived mode for editing Avro schema files."
  (setq-local web-mode-content-type "json"))


(defun web-mode-js-electric-slash (arg)
  "Insert a slash character (ARG) or JSDoc comment."
  (interactive "*P")
  (cond
   ((thing-at-point-looking-at (rx bol (* space) "//"))
    (let ((end (point)))
      (back-to-indentation)
      (let* ((indent (current-indentation))
             (comment-leader (format "%s*" (s-repeat (1+ indent) " ")))
             (existing-line
              (save-excursion
                (skip-chars-forward "/ ")
                (buffer-substring (point) end))))
        (delete-region (point) end)
        (insert "/**")
        (insert (concat "\n" comment-leader))
        (save-excursion (insert existing-line))
        (just-one-space)
        (goto-char (line-end-position))
        (save-excursion
          (insert (concat "\n" comment-leader "/"))))))
   (t
    (self-insert-command (prefix-numeric-value arg)))))


(provide 'web-mode-submodes)

;;; web-mode-submodes.el ends here
