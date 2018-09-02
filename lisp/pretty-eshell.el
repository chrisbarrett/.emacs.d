;;; pretty-eshell.el --- Utilities for defining pretty eshell prompts. -*- lexical-binding: t; -*-

;; Author: Eric Kaschalk

;;; Commentary:

;; Source:
;;   https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/display/local/pretty-eshell/pretty-eshell.el

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eshell)
(require 's)

(defvar pretty-eshell-funcs nil
  "List of `pretty-eshell-section' to enable.")

(defvar pretty-eshell-sep "  "
  "String delimits each `pretty-eshell-section'.")

(defvar pretty-eshell-section-delim " "
  "String delimits icons and their text.")

(defvar pretty-eshell-header-fun (lambda () "\n ")
  "Initial string composing the eshell prompt.")

(defvar pretty-eshell-prompt-string-fun (lambda () " $")
  "Prompt string, must match builtin `eshell-prompt-regexp'.")

(defvar pretty-eshell-prompt-num 0
  "Prompt number for current eshell session.")

(add-hook 'eshell-exit-hook
          (lambda () (setq pretty-eshell-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest _) (cl-incf pretty-eshell-prompt-num)))

;;;###autoload
(defmacro pretty-eshell-define-section (name icon form &rest props)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  ;; Roundabout way to handle case that
  ;; 1. Form is a variable and
  ;; 2. That variable might not be defined/initialized
  ;; Eg. pyvenv-virtualenv-name not loaded until pyvenv-workon
  (declare (indent 1))
  `(setq ,name
         (lambda ()
           (when (or (and (symbolp (quote ,form))
                          (bound-and-true-p ,form))
                     (and (not (symbolp (quote ,form)))
                          ,form))
             (-> ,icon
                 (concat pretty-eshell-section-delim ,form)
                 (propertize 'face (list ,@props)))))))

(defun pretty-eshell--join (acc x)
  (--if-let x
      (if (s-blank? acc)
          it
        (s-concat acc pretty-eshell-sep it))
    acc))

(defvar-local pretty-eshell--previous-section-values nil)

(defun pretty-eshell--new-prompt-sections (old-sections new-sections)
  (let ((filled-old (or old-sections (make-list (length new-sections) nil))))
    (-zip-with (lambda (old new)
                 (unless (or (equal old new) (null new) (string-blank-p new))
                   new))
               filled-old new-sections)))

;;;###autoload
(defun pretty-eshell-prompt-func ()
  "Value for `eshell-prompt-function'."
  (let* ((next-sections (seq-map #'funcall pretty-eshell-funcs))
         (filtered (-non-nil (if (null pretty-eshell--previous-section-values)
                                 next-sections
                               (pretty-eshell--new-prompt-sections pretty-eshell--previous-section-values next-sections))))
         (rendered (s-join "  " filtered)))
    (prog1 (concat (funcall pretty-eshell-header-fun)
                   (if (string-blank-p rendered) "" (concat " " rendered "\n"))
                   (funcall pretty-eshell-prompt-string-fun))
      (setq pretty-eshell--previous-section-values next-sections))))

(provide 'pretty-eshell)

;;; pretty-eshell.el ends here
