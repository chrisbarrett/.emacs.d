;;; js-lex.el --- Parse javascript into S-Expressions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; Package-Requires: ((emacs "24") (s "1.10.0") (dash "2.12.0"))
;; Version: 0.1.0

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

;; Lexer for JavaScript. It accepts ES6+, as well as Flow syntax extensions.

;;; Code:

(require 'dash)
(require 's)


;; Token type definition.

(defsubst js-lex--token (type value start end)
  (list type value start end))

(defsubst js-lex--token-type (token)
  (nth 0 token))

(defsubst js-lex--token-value (token)
  (nth 1 token))

(defsubst js-lex--token-start (token)
  (nth 2 token))

(defsubst js-lex--token-end (token)
  (nth 3 token))


;; Lexing context
;;
;; The context is a stack of symbols, representing the current context.
;; Valid contexts:
;;
;; - nil (default)
;; - interpolator

(defun js-lex--context ()
  nil)

(defun js-lex--context-push (sym context)
  (cons sym context))

(defun js-lex--context-pop (context)
  (cdr context))

(defun js-lex--context-peek (context)
  (car context))

(defun js-lex--context-interpolator-p (context)
  (equal 'interpolator (js-lex--context-peek context)))

(defun js-lex--context-enter-interpolator (context)
  (cons 'interpolator context))

(defun js-lex--context-exit-interpolator (context)
  (cl-assert (js-lex--context-interpolator-p context))
  (js-lex--context-pop context))

(defun js-lex--context-istring-p (context)
  (equal 'istring (js-lex--context-peek context)))

(defun js-lex--context-enter-istring (context)
  (cons 'istring context))

(defun js-lex--context-exit-istring (context)
  (cl-assert (js-lex--context-istring-p context))
  (js-lex--context-pop context))


(defun js-lex--failure (start-pos c1 c2)
  (-let ((line (line-number-at-pos start-pos))
         (col
          (save-excursion
            (goto-char start-pos)
            (current-column)))
         (line-start
          (save-excursion
            (goto-char start-pos)
            (line-beginning-position))))
    (error
     (s-join "\n"
             (list "lexing failure for char `%s' followed by `%s'"
                   "%s line %s column %s:"
                   (buffer-substring line-start (line-end-position))
                   "%s^"))
     (char-to-string c1)
     (char-to-string (or c2 "EOF>"))
     (or (buffer-file-name) "<input>") line col
     (s-repeat col " "))))

(defsubst js-lex--char-ident-starter-p (c)
  (or (and (>= c ?A) (<= c ?Z))
      (and (>= c ?a) (<= c ?z))
      (eq c ?_)))

(defun js-lex--goto-end-of-string-context (str-end-char context)
  (forward-char 1)
  (cl-labels ((curry-eq (c1) (lambda (c2) (eq c1 c2)))
              (eq-str-end-char (c) (eq c str-end-char)))
    (let ((escaping nil)
          (continue t))
      (while (and continue (not (eobp)))
        (pcase (char-after)
          ;; If the last char was a backslash, we can now exit the
          ;; escaping state.
          ((guard escaping)
           (setq escaping (not escaping))
           (forward-char 1))

          (?\\
           (setq escaping t)
           (forward-char 1))

          ;; Break the loop once we hit the char that terminates the string.
          ((pred eq-str-end-char)
           (when (js-lex--context-istring-p context)
             (setq context (js-lex--context-exit-istring context)))
           (setq continue nil)
           (forward-char 1))

          ;; Break the loop if we're in an interpolated string and we
          ;; find `${'
          ((guard (and (js-lex--context-istring-p context)
                       (eq (char-after) ?$)
                       (eq (char-after (1+ (point))) ?{)))
           (setq continue nil))

          (_
           (forward-char 1))))))
  context)

(defun js-lex-pop-token (start-pos offset context)
  (cl-labels
      ((token-ending-at-point
        (type context)
        (let* ((value
                (pcase type
                  ((or 'newline 'space 'tab)
                   (- (point) start-pos))
                  (_
                   (buffer-substring-no-properties start-pos (point)))))
               (start-offset (+ offset start-pos))
               (end-offset (+ offset (point))))

          (list
           :context context
           :token (js-lex--token type value start-offset end-offset)))))

    ;; Parse with two characters of lookahead.
    (let ((c1 (char-after))
          (c2 (char-after (1+ (point)))))
      (pcase c1
        ('nil
         nil)

        ;;; Interpolated string parsing.

        ;; In an interpolated string context, `${' introduces an
        ;; interpolated expression.
        ((and ?$
              (guard (eq c2 ?{))
              (guard (js-lex--context-istring-p context)))
         (let ((updated-ctx (js-lex--context-enter-interpolator context)))
           (forward-char 2)
           (token-ending-at-point 'interp-open updated-ctx)))

        ;; In an interpolated expression context, `}' re-enters
        ;; the interpolated string.
        ((and ?} (guard (js-lex--context-interpolator-p context)))
         (forward-char 1)
         (let ((updated-ctx (js-lex--context-exit-interpolator context)))
           (token-ending-at-point 'interp-close updated-ctx)))

        ;; If we're in an interpolated string, and chars from this point on
        ;; are part of the unevaluated string.
        ((guard (js-lex--context-istring-p context))
         (let ((updated-ctx (js-lex--goto-end-of-string-context ?` context)))
           (token-ending-at-point 'i-string updated-ctx)))

        ;;; Whitespace

        (?\n
         (skip-chars-forward "\n")
         (token-ending-at-point 'newline context))

        (?\t
         (skip-chars-forward "\t")
         (token-ending-at-point 'tab context))

        (32 ; Space
         (skip-chars-forward " ")
         (token-ending-at-point 'space context))

        ;;; Strings

        (?\"
         (let ((updated-ctx (js-lex--goto-end-of-string-context ?\" context)))
           (token-ending-at-point 'd-string updated-ctx)))

        (?'
         (let ((updated-ctx (js-lex--goto-end-of-string-context ?' context)))
           (token-ending-at-point 's-string updated-ctx)))

        (?`
         (let ((updated-ctx (js-lex--goto-end-of-string-context
                             ?`
                             (js-lex--context-enter-istring context))))
           (token-ending-at-point 'i-string updated-ctx)))

        ((pred js-lex--char-ident-starter-p)
         (skip-chars-forward "[:alnum:]_")
         (token-ending-at-point 'ident context))

        ;; A `/' can introduce
        ;; - a comment
        ;; - a regex (TODO)
        ;; - a division operator.
        (?/
         (pcase c2
           (?/
            (goto-char (line-end-position))
            (token-ending-at-point 'comment context))
           (?*
            (search-forward "*/")
            (token-ending-at-point 'comment context))
           (_
            (forward-char 1)
            (token-ending-at-point 'binary-op context))))

        (c1
         (js-lex--failure start-pos c1 c2))))))

(defun js-lex-buffer (&optional offset)
  "Parse the current JS buffer into an S-Expression AST.

OFFSET is added to source positions in the AST."
  (save-excursion
    (let (tokens
          (context (js-lex--context))
          (offset (or offset 0)))
      (goto-char (point-min))
      (while (unless (eobp)
               (-let [(&plist :token token :context new-context)
                      (js-lex-pop-token (point) offset context)]
                 (setq context new-context)
                 (push token tokens))))
      (nreverse tokens))))

(defun js-lex (str &optional offset)
  "Parse STR into an S-Expression AST.

OFFSET is added to source positions in the AST."
  (with-temp-buffer
    (insert str)
    (js-lex-buffer offset)))

(provide 'js-lex)

;;; js-lex.el ends here
