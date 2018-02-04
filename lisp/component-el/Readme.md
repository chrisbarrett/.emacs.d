# component.el [![Build Status](https://travis-ci.org/chrisbarrett/component-el.svg?branch=master)](https://travis-ci.org/chrisbarrett/component-el)

An implementation of declarative, reusable components for building UIs in Emacs.

I use this for my own toy projects and stuff in my personal config. YMMV.

Here's an extended example that defines a UI component for displaying data that's
loaded asynchronously.

``` emacs-lisp
;; -*- lexical-binding: t; -*-

(require 'component)

;; Define a command to asynchronously fetch a web resource, and populate some state.

(defun lorem-ipsum-fetch-async (key state)
  (let ((update-state
         (lambda (&rest _)
           (search-forward "\n\n")
           (fill-region (point) (point-max))
           (let ((str (string-trim (buffer-substring (point) (point-max)))))
             (puthash key str state)))))
    (url-retrieve "https://loripsum.net/api/2/short/plaintext" update-state nil t t)))


;; Define components to display state.

(component-define lorem-ipsum-loading ()
  `(line (propertize (face magit-dimmed) "Loading...")))

(component-define lorem-ipsum-loading-container (key state)
  (if-let* ((str (gethash key state)))
      `(line ,str)
    `(lorem-ipsum-loading)))

(component-define lorem-ipsum-section (title key state)
  `(section (,key)
            (heading ,title)
            (indent
             (lorem-ipsum-loading-container ,key ,state))))

(component-define lorem-ipsum-example (state)
  `(section (root)
            (lorem-ipsum-section "Introduction" :introduction ,state)
            (padding)
            (lorem-ipsum-section "History" :history ,state)
            (padding)
            (lorem-ipsum-section "Notes" :notes ,state)
            (padding)))


;; Define a command to start web requests and render them into a buffer.

(defun lorem-ipsum-loaded-p (state)
  (and (gethash :introduction state)
       (gethash :history state)
       (gethash :notes state)))

(defun lorem-ipsum-render (buf state)
  (when (buffer-live-p buf)
    (component-render buf `(lorem-ipsum-example ,state))
    (if (lorem-ipsum-loaded-p state)
        (message "Finished loading")
      (run-with-timer 0.3 nil #'lorem-ipsum-render buf state))))

(defun lorem-ipsum ()
  "Show a mocked up UI using placeholder text."
  (interactive)
  (let ((buf (get-buffer-create "*lorem-ipsum*"))
        (state (make-hash-table)))

    ;; Start web requests to populate state.
    (lorem-ipsum-fetch-async :introduction state)
    (lorem-ipsum-fetch-async :history state)
    (lorem-ipsum-fetch-async :notes state)

    ;; Begin the rendering sequence.
    (lorem-ipsum-render buf state)
    (display-buffer buf)))
```
