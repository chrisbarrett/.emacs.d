;;; evil-transient-state.el --- Transient evil states.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Ported from Spacemacs.

;;; Code:

(require 'hydra)

(defgroup evil-transient-state nil
  "Transient evil states."
  :group 'emulations
  :prefix "evil-transient-state-")

(defface evil-transient-state-title-face
  `((t :inherit mode-line))
  "Face for title of transient states."
  :group 'evil-transient-state)

(defun evil-transient-state--func-name (name)
  "Return the name of the transient state function.

NAME is the name of the transient STATE."
  (intern (format "%S-transient-state" name)))

(defun evil-transient-state--props-var-name (name)
  "Return the name of the variable use to store the transient state properties.
NAME is the name of the transient STATE."
  (intern (format "%S-transient-state-props" name)))

(defun evil-transient-state--body-func-name (name)
  "Return the name of the transient state function.
NAME is the name of the transient STATE."
  (intern (format "%S-transient-state/body" name)))

(defun evil-transient-state--heads-name (name)
  "Return the name of the transient state heads var containing the key bindings.
NAME is the name of the transient STATE."
  (intern (format "%S-transient-state/heads" name)))

(defun evil-transient-state--adjust-bindings (bindings to-remove to-add)
  (append
   (cl-remove-if
    (lambda (bnd)
      (and (boundp to-remove)
           (listp (symbol-value to-remove))
           (member (car bnd) (symbol-value to-remove))))
    bindings)
   (when (and (boundp to-add)
              (listp (symbol-value to-add)))
     (symbol-value to-add))))

(defun evil-transient-state--mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun evil-transient-state--create-key-binding-form (props func)
  "Helper returning a form to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader STRING'
    One or several key sequence strings to be set
    with `spacemacs-keys-set-leader-keys .

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODE is a major mode
    symbol and KEY is a key sequence string to be set with
    `spacemacs-keys-set-leader-keys-for-major-mode'.

`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'."
  (let ((evil-leader (evil-transient-state--mplist-get props :evil-leader))
        (evil-leader-for-mode (evil-transient-state--mplist-get props :evil-leader-for-mode))
        (global-key (evil-transient-state--mplist-get props :global-key))
        (def-key (evil-transient-state--mplist-get props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
           (spacemacs-keys-set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
           (spacemacs-keys-set-leader-keys-for-major-mode
             (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun evil-transient-state-make-doc
    (transient-state docstring &optional body)
  "Use `hydra' to process a docstring.

DOCSTRING is the docstring to process.

TRANSIENT-STATE is the name of the state."
  (let ((heads (evil-transient-state--heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defmacro evil-transient-state-format-hint (name var hint)
  "Format HINT and store the result in VAR for transient state NAME."
  (declare (indent 1))
  (let* ((props-var (evil-transient-state--props-var-name name))
         (prop-hint (cadr (assq 'hint props-var)))
         (prop-columns (cadr (assq 'columns props-var)))
         (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
         (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
         (prop-exit-sexp (cadr (assq 'exit-sexp props-var))))

    (eval `(setq ,var (evil-transient-state-make-doc
                       ',name
                       ,hint
                       '(nil
                         nil
                         :hint ,prop-hint
                         :columns ,prop-columns
                         :foreign-keys ,prop-foreign-keys
                         :body-pre ,prop-entry-sexp
                         :before-exit ,prop-exit-sexp))))
    'append))

(defmacro evil-transient-state-define (name &rest props)
  "Define a transient state called NAME.
NAME is a symbol.
Available PROPS:
`:on-enter SEXP'
    Evaluate SEXP when the transient state is switched on.
`:on-exit SEXP'
    Evaluate SEXP when leaving the transient state.
`:doc STRING or SEXP'
    A docstring supported by `defhydra'.
`:title STRING'
    Provide a title in the header of the transient state
`:columns INTEGER'
    Automatically generate :doc with this many number of columns.
`:hint BOOLEAN'
    Whether to display hints.  Default is nil.
`:hint-is-doc BOOLEAN'
    Whether the hints act as a documentation, the only effect of this value is
    to change where the hints are displayed.  If non-nil the hints are displayed
    on the same line as the `:title', otherwise they are displayed below it.
    Default is nil.
`:dynamic-hint SEXP'
    An sexp evaluating to a string for dynamic hinting.
    When provided `:hint' has no effect.  Default is nil.
`:foreign-keys SYMBOL'
    What to do when keys not bound in the transient state are entered.  This
    can be nil (default), which means to exit the transient state, warn,
    which means to not exit but warn the user that the key is not part
    of the transient state, or run, which means to try to run the key binding
    without exiting.
`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 DOCSTRING
                     :exit SYMBOL)
    where:
    - STRING1 is a key to be bound to the function or key map SYMBOL1.
    - DOCSTRING is a STRING or an SEXP that evaluates to a string
    - :exit SYMBOL or SEXP, if non nil then pressing this key will
      leave the transient state (default is nil).
      Important note: due to inner working of transient-maps in Emacs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.
All properties supported by `evil-transient-state--create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (evil-transient-state--func-name name))
         (props-var (evil-transient-state--props-var-name name))
         (body-func (evil-transient-state--body-func-name name))

         (add-bindings    (intern (format "evil-transient-state-%s-transient-state-add-bindings" name)))
         (remove-bindings (intern (format "evil-transient-state-%s-transient-state-remove-bindings" name)))
         (bindings (evil-transient-state--mplist-get props :bindings))

         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s/hint" func)))
         (columns (plist-get props :columns))

         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))

         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dyn-hint (plist-get props :dynamic-hint))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (evil-transient-state--create-key-binding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "Association list containing a copy of some "
                          "properties of the transient state %S. Those "
                          "properties are used in macro "
                          "`evil-transient-state-format-hint'.") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))

       (defhydra ,func
         (nil nil
              :hint ,hint
              :columns ,columns
              :foreign-keys ,foreign-keys
              :body-pre ,entry-sexp
              :before-exit ,exit-sexp)
         ,doc
         ,@(evil-transient-state--adjust-bindings
            bindings remove-bindings add-bindings))

       (when ,title
         (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)
                              "] exits state  ["
                              (if ',foreign-keys
                                  (propertize "KEY" 'face 'hydra-face-pink)
                                (propertize "KEY" 'face 'hydra-face-red))
                              "] will not exit")))
           (add-face-text-property 0 (length guide) 'italic t guide)
           (setq ,hint-var
                 (list 'concat
                       (concat
                        (propertize
                         ,title
                         'face 'evil-transient-state-title-face)
                        (if ,hint-doc-p " " "\n")) ,hint-var
                        ',dyn-hint
                        (concat "\n" guide)))))
       ,@bindkeys)))

(provide 'evil-transient-state)

;;; evil-transient-state.el ends here
