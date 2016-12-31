;;; cb-js-autoinsert.el --- Autoinsert configuration for JavaScript. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Use Autoinsert with Yasnippet to dynamically compute the skeleton to insert
;; for new JavaScript files.

;;; Code:

(require 'f)
(require 's)
(require 'subr-x)

(autoload 'yas-expand-snippet "yasnippet")

(defun cb-js-autoinsert--component-name (file)
  (s-upper-camel-case (f-no-ext (f-filename file))))


;; React components

(defun cb-js-autoinsert--flow-react-component (file)
  (format "

// @flow
import React from 'react';

type Props = {}

const ${1:%s} = ({}: Props) => (
  $0
)

export default $1;

" (cb-js-autoinsert--component-name file)))


(defun cb-js-autoinsert--react-component (file)
  (format "

import React from 'react';

const ${1:%s} = (props) => (
  $0
)

export default $1;

" (cb-js-autoinsert--component-name file)))


;; Expansion function

(defun cb-js-autoinsert-template-string ()
  (let ((file (buffer-file-name)))
    (when-let (snippet
               (cond
                ((and (member "components" (f-split file))
                      (locate-dominating-file file ".flowconfig"))
                 (cb-js-autoinsert--flow-react-component file))

                ((member "components" (f-split file))
                 (cb-js-autoinsert--react-component file))))
      (yas-expand-snippet (s-trim snippet)))))

(provide 'cb-js-autoinsert)

;;; cb-js-autoinsert.el ends here
