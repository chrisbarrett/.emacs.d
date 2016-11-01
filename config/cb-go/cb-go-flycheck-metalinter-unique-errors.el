;;; cb-go-flycheck-metalinter-unique-errors.el --- Remove duplicates from metalinter errors.   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>
;; Package-Requires: ((s "1.10.0") (flycheck "20160924.1038") (dash "2.12.1"))

;;; Commentary:

;;; Code:

(require 'dash)
(require 'flycheck)
(require 's)

(defun cb-go-flycheck-metalinter-unique-errors--message-sans-checker (err)
  (-let [(_all msg) (s-match (rx (group (+? any))
                                 (+ space) "(" (+? nonl) ")" eos)
                             (flycheck-error-message err))]
    msg))

(defun cb-go-flycheck-metalinter-unique-errors--remove-if-present (&optional errs err)
  (cond
   ((null errs)
    (list err))
   (errs
    (let ((existing (-map #'cb-go-flycheck-metalinter-unique-errors--message-sans-checker errs)))
      (cond ((-contains? existing (cb-go-flycheck-metalinter-unique-errors--message-sans-checker err))
             errs)
            (t
             (cons err errs)))))
   (t
    nil)))

(defun cb-go-flycheck-metalinter-unique-errors--filter-errors (errors)
  (let ((reduced (-reduce-from #'cb-go-flycheck-metalinter-unique-errors--remove-if-present nil errors)))
    (->
     (--map (let ((msg (cb-go-flycheck-metalinter-unique-errors--message-sans-checker it)))
              (setf (flycheck-error-message it) msg)
              it)
            reduced)
     flycheck-dedent-error-messages
     flycheck-sanitize-errors)))

(defun cb-go-flycheck-metalinter-unique-errors-init ()
  (put 'gometalinter 'flycheck-error-filter #'cb-go-flycheck-metalinter-unique-errors--filter-errors))

(provide 'cb-go-flycheck-metalinter-unique-errors)

;;; cb-go-flycheck-metalinter-unique-errors.el ends here
