;;; config-plantuml.el --- Configuration for plantuml-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; `plantuml-mode' is a major-mode for the text-based plantuml diagramming tool.

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode))
  :general
  (:keymaps 'plantuml-mode-map
   "C-c C-b" 'recompile)
  :custom
  ((plantuml-default-exec-mode 'jar)
   (plantuml-indent-level 2)
   (plantuml-jar-path (getenv "NIX_EMACS_PLANTUML_JAR")))
  :preface
  (defconst config-langs--plantuml-arrows-regexp
    (let* ((direction '(or "up" "u" "down" "d" "left" "l" "right" "r"))
           (directives '(and "[" (*? nonl) "]"))
           (lines '(+ "-"))
           (dots '(+ "."))
           ;; HACK: Make sure we have at least 2 characters in an arrow to avoid
           ;; nonsense.
           (arrow `(or  "---"
                        "..."
                        (and "<" ,lines)
                        (and "<" ,dots)
                        (and ,lines ">")
                        (and ,dots ">")
                        (and (? "<")
                             (or (and ,lines (? ,directives) (? ,direction) ,lines)
                                 (and ,dots (? ,directives) (? ,direction) ,dots))
                             (? ">")))))
      (rx-to-string `(and symbol-end (* space) ,arrow (* space) symbol-start) t)))

  :config
  (progn
    (modify-syntax-entry ?_ "w" plantuml-mode-syntax-table)

    (font-lock-add-keywords
     'plantuml-mode
     `((,(rx bol (* space) (group (or "@startuml" "@enduml")))
        (0 'font-lock-preprocessor-face))

       (,(rx bol (* space) (group "note"))
        (1 'font-lock-keyword-face)
        (,(rx (+ space) (group (or "left" "right" "bottom" "top") (+ space) (group "of")))
         nil nil
         (1 'font-lock-keyword-face)
         (2 'font-lock-keyword-face))
        (,(rx (+ space) (group (+ (syntax word))) eol)
         nil nil
         (1 'font-lock-variable-name-face)))

       (,(rx bol (* space) (group "end" (+ space) "note"))
        (1 'font-lock-keyword-face))

       (,(rx bol (* space) (group "!include" (* word)))
        (0 font-lock-keyword-face)
        (,(rx (+ nonl)) nil nil (0 'font-lock-string-face)))

       (,(rx bol (* space) (group "!startsub"))
        (0 'font-lock-preprocessor-face)
        (,(rx (+ nonl)) nil nil (0 'font-lock-function-name-face)))

       (,(rx bol (* space) (group "!endsub")) (0 'font-lock-preprocessor-face))

       ;; Improved arrows syntax highlighting

       (,(rx (group (+ (syntax word))) (group (regexp config-langs--plantuml-arrows-regexp)))
        (1 'font-lock-variable-name-face)
        (2 'font-lock-keyword-face)
        (,(rx (group (* (syntax word)))
              (group ":")
              (group (* nonl)))
         nil nil (1 'font-lock-variable-name-face) (2 'font-lock-keyword-face) (3 'font-lock-string-face)))

       ;; Groupings

       (,(rx bol (* space) (group (or "package" "node" "folder" "frame" "cloud" "database"))
             (*? nonl)
             (? (+ space) (group "as") (? (+ space) (group (+? (syntax word)))))
             (* space)
             "{" (* space) eol)
        (1 'font-lock-keyword-face)
        (2 'font-lock-keyword-face)
        (3 'font-lock-variable-name-face))

       ;; Sequence diagrams

       (,(rx bol (* space) (group (or "actor" "boundary" "control"
                                      "entity" "database" "collections"))
             (? (+ space)
                (group (+ (syntax word)))
                (* space)
                eol))
        (1 'font-lock-keyword-face)
        (2 'font-lock-variable-name-face))


       ;; Creole text formatting: https://plantuml.com/creole

       (,(rx (not "~") (group "**") (group (+? any)) (group "**"))
        (1 'parenthesis)
        (2 'bold)
        (3 'parenthesis))

       (,(rx (not "~") (group "//") (group (+? any)) (group "//"))
        (1 'parenthesis)
        (2 'italic)
        (3 'parenthesis))

       (,(rx (not "~") (group "\"\"") (group (+? any)) (group "\"\""))
        (1 'parenthesis)
        (2 'org-code)
        (3 'parenthesis))

       (,(rx (not "~") (group "--") (group (+? any)) (group "--"))
        (1 'parenthesis)
        (2 '(:strike-through t))
        (3 'parenthesis))

       (,(rx (not "~") (group "__") (group (+? any)) (group "__"))
        (1 'parenthesis)
        (2 'underline)
        (3 'parenthesis))

       (,(rx (not "~") (group "~~") (group (+? any)) (group "~~"))
        (1 'parenthesis)
        (2 '(:underline (:style wave)))
        (3 'parenthesis))

       (,(rx bol (* space) (group "*") (+ (not (any "*"))))
        (1 'org-list-dt))))))

(use-package flycheck-plantuml
  :after (:all flycheck plantuml-mode)
  :config (flycheck-plantuml-setup))

(provide 'config-plantuml)

;;; config-plantuml.el ends here
