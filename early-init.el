;;; early-init.el -- Run at the beginning of Emacs startup sequence.

;;; Commentary:

;; This file was generated by org-babel from config.org and should not be edited
;; directly.

;;; Code:


(setq package-enable-at-startup nil)


(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.7)


(when (boundp 'comp-eln-load-path)
  (let ((cache-dir "~/.cache/emacs/eln-cache/"))
    (mkdir cache-dir t)
    (add-to-list 'comp-eln-load-path cache-dir)))

