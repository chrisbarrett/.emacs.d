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
  (progn
    (defconst config-plantuml--participant-binder-rx
      `(and word-start (+? (syntax word)) word-end))

    (defconst config-plantuml--arrows-rx
      (let* ((direction '(or "up" "u" "down" "d" "left" "l" "right" "r"))
             (directives '(and "[" (*? nonl) "]"))
             (lines '(+ "-"))
             (dots '(+ ".")))
        ;; HACK: Make sure we have at least 2 characters in an arrow to avoid
        ;; nonsense.
        `(or  "---"
              "..."
              (and "<" ,lines)
              (and "<" ,dots)
              (and ,lines ">")
              (and ,dots ">")
              (and (? "<")
                   (or (and ,lines (? ,directives) (? ,direction) ,lines)
                       (and ,dots (? ,directives) (? ,direction) ,dots))
                   (? ">"))))))

  :config
  (progn
    (modify-syntax-entry ?_ "w" plantuml-mode-syntax-table)

    (font-lock-add-keywords
     'plantuml-mode
     `((,(rx bol (* space) (group (or "@startuml" "@enduml")))
        (0 'font-lock-preprocessor-face))

       (,(rx bol (* space) (group "title") symbol-end)
        (1 'font-lock-preprocessor-face))

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

       ;; Naive macro highlighting

       (,(rx bol (* space) (group upper (* (syntax word))) (* space) "("
             (? (group (+ (syntax word)))))
        (1 'font-lock-type-face)
        (2 'font-lock-variable-name-face))

       ;; Groupings

       (,(rx bol (* space) (group (or "package" "node" "folder" "frame" "cloud" "database"))
             symbol-end)
        (1 'font-lock-keyword-face)
        (,(rx symbol-start (group "as") (+ space) (group (+ (syntax word)) symbol-end))
         nil nil
         (1 'font-lock-keyword-face)
         (2 'font-lock-variable-name-face)))

       ;; Sequence diagrams

       (,(rx bol (* space) (group (or "actor" "boundary" "control"
                                      "entity" "database" "collections"))
             (? (+ space)
                (group (+ (syntax word)))
                symbol-end))
        (1 'font-lock-keyword-face)
        (2 'font-lock-variable-name-face))


       ;; Improved arrows syntax highlighting

       (,(rx-to-string `(and bol
                             (* space) (group ,config-plantuml--participant-binder-rx)
                             (* space) (group ,config-plantuml--arrows-rx)
                             (* space) (? (and (group ,config-plantuml--participant-binder-rx)
                                               (* space)
                                               (? (and
                                                   (group ":")
                                                   (group (* nonl))))))
                             eol)
                       t)
        (1 'font-lock-variable-name-face)
        (2 'font-lock-keyword-face)
        (3 'font-lock-variable-name-face)
        (4 'font-lock-keyword-face)
        (5 'font-lock-doc-face))

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
