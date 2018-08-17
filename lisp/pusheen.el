;;; pusheen.el --- Animated pusheen.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (require 'paths))

(require 'memoize)

(defvar pusheen-animation-duration-seconds 10)




(defmemoize pusheen-make-gif (filename)
  (create-image (f-join paths-assets-directory filename) 'gif))

(defvar pusheen-image-alist nil)

(defun pusheen-define (label filename)
  (add-to-list 'pusheen-image-alist `(,label . ,(pusheen-make-gif filename))))



(pusheen-define 'winky "winky.gif")
(pusheen-define 'unicorn "pusheenicorn.gif")

(defun pusheen (label)
  (let ((image (or (alist-get label pusheen-image-alist)
                   (error "Unknown pusheen :("))))
    (propertize " " 'display image)))

(defun pusheen-animate-all ()
  (dolist (image (seq-map 'cdr pusheen-image-alist))
    (image-animate image nil pusheen-animation-duration-seconds)))

(provide 'pusheen)

;;; pusheen.el ends here
