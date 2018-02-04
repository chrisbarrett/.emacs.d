# component.el

An implementation of declarable, reusable components for building UIs in Emacs.

I use this for my own toy projects and stuff in my personal config. YMMV.

Here's an extended example that defines a UI component for displaying data that's
loaded asynchronously.

``` emacs-lisp
;; -*- lexical-binding: t; -*-

(require 'component)

;; Define a command to asynchronously fetch a web resource, and populate some state.

(defconst lorem-ipsum-state (make-hash-table))

(defun lorem-ipsum-fetch-async (state-var)
  (url-retrieve "https://loripsum.net/api/2/short/plaintext"
                (lambda (&rest _)
                  (search-forward "\n\n")
                  (fill-region (point) (point-max))
                  (puthash state-var (string-trim (buffer-substring (point) (point-max))) lorem-ipsum-state))
                nil t t))

;; Define components to display that text.

(component-define lorem-ipsum-loading ()
  `(line (propertize (face magit-dimmed) "Loading...")))

(component-define lorem-ipsum-loading-container (state-var)
  (if-let* ((str (gethash state-var lorem-ipsum-state)))
      `(line ,str)
    `(lorem-ipsum-loading)))

(component-define lorem-ipsum-section (title state-var)
  `(section (,state-var)
            (heading ,title)
            (indent
             (lorem-ipsum-loading-container ,state-var))))

(component-define lorem-ipsum-example ()
  `(section (root)
            (lorem-ipsum-section "Introduction" :introduction)
            (padding)
            (lorem-ipsum-section "History" :history)
            (padding)
            (lorem-ipsum-section "Notes" :notes)
            (padding)))

;; Define a command to start web requests and render them a buffer.

(defconst lorem-ipsum-buffer-name "*lorem-ipsum*")

(defun lorem-ipsum-loaded-p ()
  (and (gethash :introduction lorem-ipsum-state)
       (gethash :history lorem-ipsum-state)
       (gethash :notes lorem-ipsum-state)))

(defun lorem-ipsum-render ()
  (when-let* ((buf (get-buffer lorem-ipsum-buffer-name)))
    (component-render buf `(lorem-ipsum-example))
    (if (lorem-ipsum-loaded-p)
        (message "Finished loading")
      (run-with-timer 0.1 nil #'lorem-ipsum-render))))

(defun lorem-ipsum ()
  "Show a mocked up UI using placeholder text."
  (interactive)

  ;; Reset state and start web requests.
  (clrhash lorem-ipsum-state)
  (lorem-ipsum-fetch-async :introduction)
  (lorem-ipsum-fetch-async :history)
  (lorem-ipsum-fetch-async :notes)

  ;; Begin rendering sequence.
  (let ((buf (get-buffer-create lorem-ipsum-buffer-name)))
    (lorem-ipsum-render)
    (display-buffer buf)))
```
