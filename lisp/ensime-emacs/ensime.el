;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs

;; Copyright (C) 2003 - 2015 the SLIME and ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;; Homepage: https://github.com/ensime/ensime-emacs
;; Keywords: languages
;; Package-Version:  1.0.1
;; Package-Requires: ((scala-mode "0.22") (sbt-mode "0.2") (yasnippet "0.9.1") (company "0.8.12") (dash "2.11.0") (s "1.10.0") (popup "0.5.3"))

;;; Commentary:
;;
;;  ENSIME has a server component which can read the AST of your
;;  project and its dependencies, providing features that are simply
;;  not possible with emacs-lisp pattern matching.
;;
;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'url-gw)
(require 'dash)
(require 'arc-mode)
(require 'thingatpt)
(require 'comint)
(require 'timer)
(require 'tooltip)
(require 'pp)
(require 'hideshow)
(require 'flymake)
(require 'font-lock)
(require 'easymenu)
(require 'ensime-client)
(require 'ensime-util)
(require 'ensime-vars)
(require 'ensime-config)
(require 'ensime-completion-util)

(require 'ensime-inf)
(require 'ensime-stacktrace)
(require 'ensime-debug)
(require 'ensime-editor)
(require 'ensime-goto-testfile)
(require 'ensime-inspector)
(require 'ensime-mode)
(require 'ensime-model)
(require 'ensime-notes)
(require 'ensime-popup)
(require 'ensime-refactor)
(require 'ensime-startup)
(require 'ensime-undo)
(require 'ensime-search)
(require 'ensime-doc)
(require 'ensime-semantic-highlight)
(require 'ensime-ui)
(require 'ensime-http)
(require 'timer)

;; should really be optional
(require 'ensime-sbt)

;; autoload ensime-ac-enable and ensime-company-enable so that the
;; user can select which backend to use without loading both.
(autoload 'ensime-company-enable "ensime-company")
(autoload 'ensime-ac-enable "ensime-auto-complete")

(defvar ensime-protocol-version "1.0")

(defvar ensime-prefer-noninteractive t
  "State variable used for regression testing, and for skipping prompt in conjunction with sbt.")

(defvar ensime-popup-in-other-frame nil)


;;;###autoload
(defun ensime ()
  "Read config file for settings then start an ensime-server and connect."
  (interactive)
  (ensime-startup-notifications)
  (let ((orig-bfn buffer-file-name))
    (condition-case ex
        (if ensime-auto-generate-config
            (ensime--maybe-refresh-config
             nil
             `(lambda () (ensime--maybe-update-and-start-noninteractive ,orig-bfn))
             `(lambda (reason) (ensime--maybe-update-and-start-noninteractive ,orig-bfn)))
          (ensime--maybe-update-and-start orig-bfn))
      ('error (error (format
                      "check that sbt is on your PATH and see the Troubleshooting Guide for further steps %s [%s]"
                      "http://ensime.org/editors/emacs/troubleshooting/" ex))))))

;;;###autoload
(defun ensime-remote (host port)
  "Read config file for settings. Then connect to an existing ENSIME server."
  (interactive "shost: \nnport: ")

  (let ((orig-buffer-file-name buffer-file-name))
    (if ensime-auto-generate-config
        (ensime--maybe-refresh-config
         nil
         `(lambda () (ensime--maybe-update-and-start orig-buffer-file-name (url-gateway-nslookup-host ,host) ,port))
         `(lambda (reason) (ensime--maybe-update-and-start orig-buffer-file-name (url-gateway-nslookup-host ,host) ,port))))))

(provide 'ensime)

;;; ensime.el ends here
