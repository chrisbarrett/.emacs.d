;;; component.el --- A declarative component DSL.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))

;; Version: 0.0.1

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

;; A DSL for building and instantiating reusable magit-style Emacs UI
;; components.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'subr-x)

;; Derived component support.

(defvar component--components (make-hash-table :test #'eq)
  "A mapping from the name of a component to its interpretation function.

When traversing a component AST any list beginning with a symbol
is interpreted as a component reference by name. That symbol is
used to look up an interpretation function in this table. That
function is applied to any remaining elements of that cons.

The result of a function in this hash-table should be a new
component AST, or a string value to be inserted directly.")

(defmacro component-define (name arglist &rest body)
  "Define a rendering component.

NAME is the name of the component, which may thereafter be
referenced directly in component ASTs.

ARGLIST is the arguments that must be supplied to construct the
component.

BODY is the definition of the component."
  (declare (indent 2))
  (cl-assert (symbolp name))
  (cl-assert (listp arglist))
  (let ((fname (intern (format "component--generated--%s" name)))
        (docstring (format "Auto-generated component constructor function.

Creates instances of %s components, which may be referred to as
such in component ASTs." name)))
    `(progn
       (cl-defun ,fname ,arglist ,docstring ,@body)
       (puthash ',name #',fname component--components))))

(component-define line (inner-ast)
  `(,inner-ast
    (padding)))

(component-define key-value (width key value)
  (cl-assert (numberp width) t)
  (cl-assert (<= 0 width) t)
  (cl-assert (stringp key) t)
  (when value
    (let* ((fmt-string (concat "%-" (number-to-string width) "s"))
           (value-str (with-temp-buffer
                        (component-eval value)
                        (buffer-string)))
           (str (concat (propertize (format fmt-string (concat key ": ")) 'face 'magit-header-line)
                        value-str)))
      (unless (string-blank-p (buffer-substring (line-beginning-position) (line-end-position)))
        (newline))
      `(copy-prop ,(substring-no-properties value-str) (line ,str)))))

(component-define copy-prop (copy-str &rest inner-ast)
  `(propertize (component-copy ,copy-str)
               ,inner-ast))


;; Component AST interpreter.

(defconst component--indentation-width 2)
(defconst component--space ?\ )

(defsubst component--indentation (indent-level)
  (make-string (* indent-level component--indentation-width) component--space))

(defsubst component--eval-string (s indent-level)
  (let ((value (if (string-empty-p (buffer-substring (line-beginning-position) (point)))
                   (concat (component--indentation indent-level) s)
                 s)))
    (insert value)))

(defsubst component--finalize-heading (start-pos)
  ;; This implementation is adapted from `magit-insert-heading'.

  ;; Apply heading face if no other face is set.
  (let ((heading (buffer-substring start-pos (line-end-position))))
    (unless (next-single-property-change 0 'face (concat "0" heading))
      (add-text-properties start-pos (point) '(face magit-section-heading))))
  (unless (bolp)
    (insert ?\n))

  ;; Update containing section to point to this heading.
  (setf (magit-section-content magit-insert-section--current) (point-marker)))

(defsubst component--finalize-list-item (start-pos)
  (save-excursion
    (goto-char start-pos)
    (goto-char (line-beginning-position))
    (skip-chars-forward " ")
    (unless (eq (char-after) ?-)
      (delete-char -2)
      (insert "- "))))

(defun component--append-sentinel (instructions sentinel)
  (append (list instructions) (list sentinel)))

(defun component-eval (component &optional indent-level)
  "Draw COMPONENT into the current buffer, optionally at indentation level INDENT-LEVEL."

  ;; The evaluator is implemented as a loop over an instruction stack. The
  ;; `instruction-stack' variable is a stack of AST instructions, the head of
  ;; which is the instruction to interpret. Its initial value is set to the
  ;; input to this function. After an instruction is interpreted, the item at
  ;; the top of the stack is popped. The loop ends when there are no more
  ;; instructions on the stack.
  ;;
  ;; If nested instructions are encountered in the AST, they are pushed onto the
  ;; stack, generally with a sentinel instruction to restore previous
  ;; interpreter state.

  (let ((instruction-stack (list component))
        (indent-level (or indent-level 0)))

    (while instruction-stack
      (pcase (car instruction-stack)

        ;; Strings are inserted directly, possibly with indentation.

        ((and (pred stringp) s)
         (component--eval-string s indent-level)
         (!cdr instruction-stack))

        ;; Padding gets some special error checking to make sure it has no inner
        ;; AST, since I get `padding' and `indent' mixed up all the time.

        ((and `(padding . ,_rest) (guard _rest))
         (error "Padding takes no arguments"))
        (`(padding)
         (newline)
         (!cdr instruction-stack))

        ;; Indentation
        ;;
        ;; The current indentation level is tracked by the interpreter. When an
        ;; `indent' directive is encountered, the indent level is incremented
        ;; and the inner AST is pushed to the stack with a sentinel appended.
        ;; When the sentinel is encountered, the indentation level is decreased.

        (`(indent . ,inner-ast)
         (let ((next (component--append-sentinel inner-ast 'component--indent-sentinel)))
           (setq indent-level (1+ indent-level))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`component--indent-sentinel
         (setq indent-level (1- indent-level))
         (!cdr instruction-stack))

        ;; Properties
        ;;
        ;; To propertize some inserted text, the inner AST is pushed to the
        ;; stack with a sentinel appended. The sentinel records the properties
        ;; to apply and the start position of the span. Once the sentinel is
        ;; encountered, the end position of the span is known and properties can
        ;; be applied.

        (`(propertize ,spec . ,inner-ast)
         (let ((next (component--append-sentinel inner-ast `(component--propertize-sentinel ,(point) ,spec))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(component--propertize-sentinel ,start ,spec)
         (add-text-properties start (point) spec)
         (!cdr instruction-stack))

        ;; Bulleted lists
        ;;
        ;; A bulleted list is decomposed into a sequence of instructions, each
        ;; of which tracks its buffer positions using sentinel values.
        ;;
        ;; The bullet group is indented, and each item's start position is
        ;; recorded in a sentinel value. When an item's sentinel is encountered,
        ;; the item's dash is written to the buffer.

        (`(list . ,items)
         (let ((next `(indent ,@(--map `(component--list-item . ,it) items))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(component--list-item . ,inner-ast)
         (let ((next (component--append-sentinel inner-ast `(component--list-item-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(component--list-item-sentinel . ,start)
         (component--finalize-list-item start)
         (!cdr instruction-stack))

        ;; Headings
        ;;
        ;; Heading insertion requires interpretation of an inner AST to build
        ;; the heading text. A special sentinel is appended to the inner AST
        ;; that tells the interpreter to finalise the heading after interpreting
        ;; the inner value.

        (`(heading ,inner-ast)
         (unless magit-insert-section--current (error "Eval AST: Inserting a heading, but not in a section"))
         (let ((next (component--append-sentinel inner-ast `(component--heading-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(component--heading-sentinel . ,start-pos)
         (component--finalize-heading start-pos)
         (!cdr instruction-stack))

        ;; Sections
        ;;
        ;; KLUDGE: The section insertion logic in magit has complex state. It's
        ;; easier just to evaluate recursively than try to reproduce that logic
        ;; in the interpreter. This is safe so long as section nesting doesn't
        ;; approach `max-lisp-eval-depth'.

        (`(section (,sym . ,args) . ,inner)
         (!cdr instruction-stack)
         (let ((hiddenp (car args)))
           (eval `(magit-insert-section (,sym nil ,hiddenp)
                    (component-eval ',inner ,indent-level)))))

        ;; Custom components
        ;;
        ;; If the current instruction is a list and its head is a symbol, look
        ;; it up in the component definition table. If the lookup succeeds,
        ;; evaluate the component's constructor function to derive an AST, and
        ;; push that AST onto the stack.

        ((and `(,component . ,args)
              (guard component)
              (guard (symbolp component)))
         (!cdr instruction-stack)

         (if-let* ((constructor (gethash component component--components)))
             (!cons (apply constructor args) instruction-stack)
           (error "Component not defined: %s" component)))

        ;; Lists of instructions
        ;;
        ;; If the list being scrutinised does not begin with a symbol, it is
        ;; assumed to be a sequence of instructions. The items are pushed to the
        ;; stack.

        ((and (pred listp) actions)
         (!cdr instruction-stack)
         (setq instruction-stack (append actions instruction-stack)))

        ;; Heck, you've done the interpreter a frighten.

        (other
         (message "Stack: %s" instruction-stack)
         (error "Unknown AST instruction: %s" other))))))


;; Rendering

(defmacro component--save-window-state (&rest body)
  "Restore window state after executing BODY.

This is useful if the buffer is erased and repopulated in BODY,
in which case `save-excursion' is insufficient to restore the
window state."
  `(let ((window-start-line (window-start))
         (inhibit-redisplay t))
     (prog1 (progn ,@body)
       (set-window-start (selected-window) window-start-line))))

(defun component-render (buffer ast)
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (initial-col (current-column))
           (initial-line (line-number-at-pos))
           (section-restoration-args
            (when-let* ((section (magit-current-section)))
              (nconc (list section)
                     (magit-refresh-get-relative-position)))))

      (deactivate-mark)
      (setq magit-section-highlight-overlays nil)
      (setq magit-section-highlighted-section nil)
      (setq magit-section-highlighted-sections nil)
      (setq magit-section-unhighlight-sections nil)

      (component--save-window-state
       (erase-buffer)
       (component-eval ast)

       (if section-restoration-args
           (apply #'magit-section-goto-successor section-restoration-args)
         (goto-char (point-min))
         (forward-line (1- initial-line)))

       (move-to-column initial-col)
       (magit-section-update-highlight)))))


(provide 'component)

;;; component.el ends here
