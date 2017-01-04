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

(defun cb-js-autoinsert--container-name (file)
  (s-upper-camel-case (f-no-ext (f-filename file))))

(defun cb-js-autoinsert--component-name (file)
  (let ((container-name (s-upper-camel-case (f-no-ext (f-filename file)))))
    (s-chop-suffix "Component" container-name)))


;; React components and containers

(defun cb-js-autoinsert--flow-react-container (file)
  (let ((container-name (cb-js-autoinsert--container-name file))
        (component-name (cb-js-autoinsert--component-name file)))
    (concat "

// @flow
import React from 'react';

type Props = {| |}

const " component-name " = (props: Props) => (
  <p>" component-name "</p>
);

class " container-name " extends React.Component {
  props: Props;

  render = () => {
    return (
      <" component-name " $0 />
    )
  };
}

export default " container-name ";

")))

(defun cb-js-autoinsert--flow-react-component (file)
  (let ((component-name (cb-js-autoinsert--component-name file)))
    (concat "

// @flow
import React from 'react';

type Props = {| |}

const " component-name " = (props: Props) => (
  <p>$0</p>
);

export default " component-name ";

")))

(defun cb-js-autoinsert--react-container (file)
  (let ((container-name (cb-js-autoinsert--container-name file))
        (component-name (cb-js-autoinsert--component-name file)))
    (concat "

import React from 'react';

const " component-name " = (props) => (
  <p>" component-name "</p>
);

class " container-name " extends React.Component {
  render = () => {
    return (
      <" component-name " $0 />
    )
  };
}

export default " container-name ";

")))

(defun cb-js-autoinsert--react-component (file)
  (let ((component-name (cb-js-autoinsert--container-name file)))
    (concat "

import React from 'react';

const " component-name " = (props) => (
  <p>$0</p>
);

export default " component-name ";

")))

(defun cb-js-autoinsert--flow-standard-source ()
  nil
  "
// @flow

$0

")


;; Expansion function

(defun cb-js-autoinsert-template-string ()
  (let* ((file (buffer-file-name))
         (container-file-name? (s-suffix? "Container" (f-no-ext (f-filename file))))
         (in-components? (member "components" (f-split file)))
         (flow-project? (locate-dominating-file file ".flowconfig")))
    (when-let (snippet
               (cond
                ((and in-components? flow-project? container-file-name?)
                 (cb-js-autoinsert--flow-react-container file))
                ((and in-components? flow-project?)
                 (cb-js-autoinsert--flow-react-component file))

                ((and in-components? container-file-name?)
                 (cb-js-autoinsert--flow-react-container file))
                (in-components?
                 (cb-js-autoinsert--flow-react-component file))

                (flow-project?
                 (cb-js-autoinsert--flow-standard-source))))

      (yas-expand-snippet (s-trim snippet)))))

(provide 'cb-js-autoinsert)

;;; cb-js-autoinsert.el ends here
