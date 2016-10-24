(require 'f)

(defvar ensime-emacs-support-path
  (f-dirname load-file-name))

(defvar ensime-emacs-features-path
  (f-parent ensime-emacs-support-path))

(defvar ensime-emacs-root-path
  (f-parent ensime-emacs-features-path))

(add-to-list 'load-path ensime-emacs-root-path)

(require 'ensime)
(require 'ensime-helm)
(require 'espuds)
(require 'scala-mode)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
