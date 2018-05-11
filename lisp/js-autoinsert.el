;;; js-autoinsert.el --- Autoinsert configuration for JavaScript. -*- lexical-binding: t; -*-

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

(defun js-autoinsert--file-no-ext (file)
  (f-no-ext (f-filename file)))

(defun js-autoinsert--screaming-snake (s)
  (upcase (s-snake-case s)))

(defun js-autoinsert--container-name (file)
  (s-upper-camel-case (js-autoinsert--file-no-ext file)))

(defun js-autoinsert--component-name (file)
  (s-chop-suffix "Container"
                 (s-upper-camel-case (js-autoinsert--file-no-ext file))))

(defun js-autoinsert--reducer-name (file)
  (s-lower-camel-case (js-autoinsert--file-no-ext file)))

;; React components and containers

(defun js-autoinsert--flow-react-container (file)
  (let ((container-name (js-autoinsert--container-name file))
        (component-name (js-autoinsert--component-name file)))
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

(defun js-autoinsert--flow-react-component (file)
  (let ((component-name (js-autoinsert--component-name file)))
    (concat "

// @flow
import React from 'react';

type Props = {| |};

const " component-name " = (props: Props) => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

export default " component-name ";

")))

(defun js-autoinsert--react-container (file)
  (let ((container-name (js-autoinsert--container-name file))
        (component-name (js-autoinsert--component-name file)))
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

(defun js-autoinsert--react-component (file)
  (let ((component-name (js-autoinsert--component-name file)))
    (concat "

import React from 'react';

const " component-name " = props => (
  <div className=\"" (s-snake-case component-name) "\">" component-name "</div>$0
);

export default " component-name ";

")))

(defun js-autoinsert--flow-standard-source ()
  nil
  "
// @flow

$0

")

;; Redux reducers

(defun js-autoinsert--flow-redux-reducer (file)
  (let ((function-name (js-autoinsert--reducer-name file)))
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

(defun js-autoinsert--redux-reducer (file)
  (let ((function-name (js-autoinsert--reducer-name file)))
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

(defun js-autoinsert--flow-redux-action (file)
  (let ((filename (s-snake-case (js-autoinsert--file-no-ext file))))
    (concat "

// @flow

export type Action = {| type: '${1:CREATE_" filename "$(upcase yas/text)}' |}

// Action functions

export const ${1:$(s-lower-camel-case yas/text)} = (): Action => ({
  type: ${1:$(js-autoinsert--screaming-snake yas/text)},$0
});

")))

(defun js-autoinsert--redux-action (file)
  (let ((filename (s-snake-case (js-autoinsert--file-no-ext file))))
    (concat "

// Action type labels

export const ${1:CREATE_" filename "$(upcase yas/text)} = '${1:$(js-autoinsert--screaming-snake yas/text)}';


// Action functions

export const ${1:$(s-lower-camel-case yas/text)} = () => ({
  type: ${1:$(js-autoinsert--screaming-snake yas/text)},$0
});

")))

;; Tests

(defun js-autoinsert--flow-jest-test ()
  (concat "

// @flow

test('${1:feature}', () => {
  $0
});

"))

(defun js-autoinsert--jest-test ()
  (concat "

test('${1:feature}', () => {
  $0
});

"))

;; Expansion function

(defun js-autoinsert-template-string ()
  (let* ((file (buffer-file-name))
         (container-file-name? (s-suffix? "Container" (f-no-ext (f-filename file))))
         (test? (or (s-ends-with? ".test.js" file)
                    (seq-intersection '("__tests__" "tests" "test") (f-split file))))
         (react-component? (member "components" (f-split file)))
         (redux-reducer? (member "reducers" (f-split file)))
         (redux-action? (member "actions" (f-split file)))
         (flow-project? (locate-dominating-file file ".flowconfig")))
    (when-let* ((snippet
                 (cond
                  ((and test? flow-project?)
                   (js-autoinsert--flow-jest-test))
                  (test?
                   (js-autoinsert--jest-test))

                  ((and react-component? flow-project? container-file-name?)
                   (js-autoinsert--flow-react-container file))
                  ((and react-component? flow-project?)
                   (js-autoinsert--flow-react-component file))

                  ((and react-component? container-file-name?)
                   (js-autoinsert--flow-react-container file))
                  (react-component?
                   (js-autoinsert--flow-react-component file))

                  ((and redux-reducer? flow-project?)
                   (js-autoinsert--flow-redux-reducer file))
                  (redux-reducer?
                   (js-autoinsert--redux-reducer file))

                  ((and redux-action? flow-project?)
                   (js-autoinsert--flow-redux-action file))
                  (redux-action?
                   (js-autoinsert--redux-action file))

                  (flow-project?
                   (js-autoinsert--flow-standard-source)))))

      (yas-expand-snippet (s-trim snippet)))))

(provide 'js-autoinsert)

;;; js-autoinsert.el ends here
