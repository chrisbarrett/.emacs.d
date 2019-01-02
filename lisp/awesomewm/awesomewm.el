;;; awesomewm.el --- Interop with AwesomeWM  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

(autoload 'compile-goto-error "compile")



(defconst awesomewm-client-executable "awesome-client")


;; Implement a limited Elisp to Lua compiler.
;;
;; Basic features:
;;
;; - Numeric and string literals
;;
;;     `1`, `"hello"`
;;
;; - Hash literals use a vector syntax:
;;
;;     `[1 2 3]` -> `{1, 2, 3}`
;;
;;     `[(foo . 1) (bar . 2)]` -> `{foo = 1, bar = 2}`
;;
;; - Function application
;;
;;     `(foo bar baz)` -> `foo(bar, baz)`
;;
;;     `(foo.bar baz)` -> `foo.bar(baz)`
;;
;; - Variable binding
;;
;;      ```
;;      (let ((x 1)
;;            (y 2))
;;        (foo x y))
;;      ```
;;
;;      ->
;;
;;      ```
;;      local x = 1
;;      local y = 2
;;      foo(x, y)
;;      ```

(defun awesomewm--compile-hash-entry (entry)
  (pcase entry
    (`(,name . ,value)
     (format "%s = %s" (awesomewm--sanitised-symbol name) (awesomewm-compile value)))
    (_
     (awesomewm-compile entry))))

(defun awesomewm--compile-binding (kvp)
  (pcase kvp
    (`(,name ,value)
     (format "local %s = %s" (awesomewm--sanitised-symbol name) (awesomewm-compile value)))
    (_
     (error "Illegal binding: %s" kvp))))

(defun awesomewm--sanitised-symbol (symbol)
  (replace-regexp-in-string "-" "_" (prin1-to-string symbol t)))

(defun awesomewm-compile (term)
  (pcase term
    ((pred symbolp)
     (awesomewm--sanitised-symbol term))
    ((pred stringp)
     (replace-regexp-in-string "\n" "\\n" (prin1-to-string term) nil t))
    ((pred numberp)
     (prin1-to-string term))
    ((pred vectorp)
     (format "{%s}" (string-join (seq-map #'awesomewm--compile-hash-entry term) ", ")))
    (`(let ,bindings . ,body)
     (let ((statements
            (seq-filter #'identity (append (seq-map #'awesomewm--compile-binding bindings)
                                           (seq-map #'awesomewm-compile body)))))
       (string-join statements "\n")))
    (`(,ident . ,args)
     (format "%s(%s)" ident (string-join (seq-map #'awesomewm-compile args) ", ")))
    (_
     (error "Illegal term: %s" term))))



(defun awesomewm-exec (dsl)
  "Compile DSL to Lua and send to the awesome client."
  (let ((lua (awesomewm-compile dsl)))
    (start-process "*awesome-client*" "*awesome-client*" awesomewm-client-executable lua)))



(defun awesomewm-goto-error (path)
  "Open the last error shown by Awesome in a compilation buffer.

PATH is the path to the output file."
  (with-current-buffer (find-file path)
    (compilation-minor-mode)
    (add-hook 'kill-buffer-hook (lambda () (unwind-protect (delete-file path))) nil t)
    (prog1 (current-buffer)
      (compile-goto-error))))

(defun awesomewm-notify (title &optional text)
  "Show a Naughty notification.

TITLE is the title text, which is shown prominently.

TEXT is smaller, and shown below the title."
  (awesomewm-exec
   `(let ((n (require "naughty")))
      (n.notify [(title . ,title) (text . ,text)]))))

(defun awesomewm-notify-error (title &optional text)
  "Show an error-level Naughty notification.

TITLE is the title text, which is shown prominently.

TEXT is smaller, and shown below the title."
  (awesomewm-exec
   `(let ((n (require "naughty"))
          (error-preset n.config.presets.critical))
      (n.notify [(title . ,title) (text . ,text) (preset . error-preset)]))))

(provide 'awesomewm)

;;; awesomewm.el ends here
