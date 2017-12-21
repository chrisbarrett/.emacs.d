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

(defun cb-js-autoinsert--file-no-ext (file)
  (f-no-ext (f-filename file)))

(defun cb-js-autoinsert--screaming-snake (s)
  (upcase (s-snake-case s)))

(defun cb-js-autoinsert--container-name (file)
  (s-upper-camel-case (cb-js-autoinsert--file-no-ext file)))

(defun cb-js-autoinsert--component-name (file)
  (s-chop-suffix "Container"
                 (s-upper-camel-case (cb-js-autoinsert--file-no-ext file))))

(defun cb-js-autoinsert--reducer-name (file)
  (s-lower-camel-case (cb-js-autoinsert--file-no-ext file)))

;; React components and containers

(defun cb-js-autoinsert--flow-react-container (file)
  (let ((container-name (cb-js-autoinsert--container-name file))
        (component-name (cb-js-autoinsert--component-name file)))
    (concat "

// @flow
import React from 'react';
import { connect } from 'react-redux';

import type { State } from '../types';
import type { Action } from '../store/actions';

// Presentational component

type Props = {| |};

const " component-name " = (props: Props) => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

// Container component

const stateToProps = (state: State): $Shape<Props> => ({});

const dispatchToProps = (dispatch: Action => void): $Shape<Props> => ({});

const " container-name " = connect(stateToProps, dispatchToProps)(" component-name ");

export default " container-name ";

")))

(defun cb-js-autoinsert--flow-react-component (file)
  (let ((component-name (cb-js-autoinsert--component-name file)))
    (concat "

// @flow
import React from 'react';

type Props = {| |};

const " component-name " = (props: Props) => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

export default " component-name ";

")))

(defun cb-js-autoinsert--react-container (file)
  (let ((container-name (cb-js-autoinsert--container-name file))
        (component-name (cb-js-autoinsert--component-name file)))
    (concat "

import React from 'react';
import { connect } from 'react-redux';

// Presentational component

const " component-name " = props => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

// Container component

const stateToProps = state => ({});

const dispatchToProps = dispatch => ({});

const " container-name " = connect(stateToProps, dispatchToProps)(" component-name ");

export default " container-name ";

")))

(defun cb-js-autoinsert--react-component (file)
  (let ((component-name (cb-js-autoinsert--component-name file)))
    (concat "

import React from 'react';

const " component-name " = props => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

export default " component-name ";

")))

(defun cb-js-autoinsert--flow-standard-source ()
  nil
  "
// @flow

$0

")

;; Redux reducers

(defun cb-js-autoinsert--flow-redux-reducer (file)
  (let ((function-name (cb-js-autoinsert--reducer-name file)))
    (concat "

// @flow
import type { Action } from './actions';

type State = {| |};

const initialState: State = {};

const " function-name " = (state: State = initialState, action: Action): State => {
  switch (action.type) {
    case $0:
      return state;

    default:
      return state;
  }
};

export default " function-name ";

")))

(defun cb-js-autoinsert--redux-reducer (file)
  (let ((function-name (cb-js-autoinsert--reducer-name file)))
    (concat "

import Actions from './actions';

const initialState = {};

const " function-name " = (state = initialState, action) => {
  switch (action.type) {
    default:
      return state;
  }
};

export default " function-name ";

")))

;; Redux actions

(defun cb-js-autoinsert--flow-redux-action (file)
  (let ((filename (s-snake-case (cb-js-autoinsert--file-no-ext file))))
    (concat "

// @flow

export type Action = {| type: '${1:CREATE_" filename "$(upcase yas/text)}' |}

// Action functions

export const ${1:$(s-lower-camel-case yas/text)} = (): Action => ({
  type: ${1:$(cb-js-autoinsert--screaming-snake yas/text)},$0
});

")))

(defun cb-js-autoinsert--redux-action (file)
  (let ((filename (s-snake-case (cb-js-autoinsert--file-no-ext file))))
    (concat "

// Action type labels

export const ${1:CREATE_" filename "$(upcase yas/text)} = '${1:$(cb-js-autoinsert--screaming-snake yas/text)}';


// Action functions

export const ${1:$(s-lower-camel-case yas/text)} = () => ({
  type: ${1:$(cb-js-autoinsert--screaming-snake yas/text)},$0
});

")))

;; Expansion function

(defun cb-js-autoinsert-template-string ()
  (let* ((file (buffer-file-name))
         (container-file-name? (s-suffix? "Container" (f-no-ext (f-filename file))))
         (react-component? (member "components" (f-split file)))
         (redux-reducer? (member "reducers" (f-split file)))
         (redux-action? (member "actions" (f-split file)))
         (flow-project? (locate-dominating-file file ".flowconfig")))
    (when-let* ((snippet
                 (cond
                  ((and react-component? flow-project? container-file-name?)
                   (cb-js-autoinsert--flow-react-container file))
                  ((and react-component? flow-project?)
                   (cb-js-autoinsert--flow-react-component file))

                  ((and react-component? container-file-name?)
                   (cb-js-autoinsert--flow-react-container file))
                  (react-component?
                   (cb-js-autoinsert--flow-react-component file))

                  ((and redux-reducer? flow-project?)
                   (cb-js-autoinsert--flow-redux-reducer file))
                  (redux-reducer?
                   (cb-js-autoinsert--redux-reducer file))

                  ((and redux-action? flow-project?)
                   (cb-js-autoinsert--flow-redux-action file))
                  (redux-action?
                   (cb-js-autoinsert--redux-action file))

                  (flow-project?
                   (cb-js-autoinsert--flow-standard-source)))))

      (yas-expand-snippet (s-trim snippet)))))

(provide 'cb-js-autoinsert)

;;; cb-js-autoinsert.el ends here
