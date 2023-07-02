;;; better-eval-expression.el --- A better eval expression implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Barrett

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

;; Define an alternative version of =eval-expression= that uses
;; `emacs-lisp-mode' to provide font-locking, and handles `smartparens' better.

;; See:
;; - https://lists.gnu.org/archive/html/help-gnu-emacs/2014-07/msg00135.html

;;; Code:

(require 'pp)

(defvar better-eval-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map (kbd "<escape>") #'abort-minibuffers)
    (define-key map (kbd "C-g") #'abort-minibuffers)
    map))

(defun better-eval-expression--read (prompt &optional initial-contents)
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((inhibit-message t))
          (emacs-lisp-mode)
          (when (bound-and-true-p flymake-mode)
            (flymake-mode -1))
          (use-local-map better-eval-expression-map)
          (setq font-lock-mode t)
          (funcall font-lock-function 1)))
    (read-from-minibuffer prompt initial-contents
                          better-eval-expression-map nil
                          'read-expression-history)))

;;;###autoload
(defun better-eval-expression (expression &optional arg)
  "Like `eval-expression' with nicer input handling.

- Use `emacs-lisp-mode' to provide font locking and better
  integration with other packages.

- Use the `pp' library to display the output in a readable form.

EXPRESSION is a Lisp form to evaluate.

With optional prefix ARG, insert the results into the buffer at
point."
  (interactive (list (read (better-eval-expression--read "Eval: "))
                     current-prefix-arg))
  (condition-case _
      (if arg
          (insert (pp-to-string (eval expression lexical-binding)))
        (pp-display-expression (eval expression lexical-binding)
                               "*Pp Eval Output*" t))
    (error
     (eval-expression expression arg))))

(provide 'better-eval-expression)

;;; better-eval-expression.el ends here
