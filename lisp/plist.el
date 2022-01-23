;;; plist.el --- Utilities for working with plists  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dash)
(require 'ht)

(defun plist-keys (plist)
  (seq-map #'car (-partition-all 2 plist)))

(defun plist-pick (keys plist)
  (let ((res (ht-create)))
    (dolist (key keys)
      (puthash key (plist-get plist key) res))
    (ht-to-plist res)))

(defun plist-map-keys (fn plist)
  (let ((pairs (ht-map (lambda (k v)
                         (list (funcall fn k) v))
                       (ht-from-plist plist))))
    (apply 'append pairs)))

(defun plist--pred-name-for-type (type)
  (intern (format "%s-p" type)))

(defmacro plist-define-predicate (type required-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  `(defun ,(plist--pred-name-for-type type) (value)
     (when (listp value)
       (let ((required-keys ',required-keys))
         (let ((keys (plist-keys value)))
           (and (null (seq-difference required-keys keys))
                (seq-every-p (lambda (key)
                               (plist-get value key))
                             required-keys)))))))

(defun plist--validator-for-type (type)
  (intern (format "%s-assert" type)))

(defmacro plist-define-validator (type required-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  `(defsubst ,(plist--validator-for-type type) (value)
     (cl-assert (listp value) t "Expected a plist" )
     (let ((required-keys ',required-keys)
           (keys (plist-keys value)))
       (cl-assert (null (seq-difference required-keys keys)) t "Missing required keys: %s" (seq-difference required-keys keys))
       (cl-assert (seq-every-p (lambda (key)
                                 (plist-get value key))
                               required-keys) t
                               "Illegal values for required keys: %s" (seq-filter (lambda (key)
                                                                                    (null (plist-get value key)))
                                                                                  required-keys)))))

(defun plist--pred-accessor-name (type keyword)
  (intern (format "%s-%s" type (string-remove-prefix ":" (symbol-name keyword)))))

(defmacro plist-define-getter (type key)
  (cl-assert (symbolp type))
  (cl-assert (keywordp key))
  (let ((validator (plist--validator-for-type type)))
    `(defun ,(plist--pred-accessor-name type key) (,type)
       ,(format "Lookup `%s' in a plist of type `%s'." key type)
       (when (fboundp ',validator)
         (,validator ,type))
       (plist-get ,type ,key))))

(defmacro plist-define-create (type keys)
  (cl-assert (symbolp type))
  (cl-assert (listp keys))
  `(defun ,(intern (format "%s-create" type)) (&rest attrs)
     (,(plist--validator-for-type type) attrs)
     (plist-pick ',keys attrs)))

(cl-defmacro plist-define (type &key required optional)
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  (let ((keys (seq-union required optional)))
    `(progn
       (plist-define-predicate ,type ,required)
       (plist-define-validator ,type ,required)
       (plist-define-create ,type ,keys)
       ,@(seq-map (lambda (it) `(plist-define-getter ,type ,it))
                  keys))))

(provide 'plist)

;;; plist.el ends here
