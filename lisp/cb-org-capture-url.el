;;; cb-org-capture-url.el --- Utilities for capturing URLs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(autoload 'thing-at-point-url-at-point "thingatpt")

(defun cb-org-capture-url--decode-html-entities (str)
  (with-temp-buffer
    (insert str)
    (pcase (libxml-parse-html-region (point-min) (point-max))
      (`(html nil
              (body nil
                    (p nil
                       ,(and (pred stringp) decoded))))
       decoded))))

(defun cb-org-capture-url--parse-html-title (html)
  "Extract the title from an HTML document."
  (-let (((_ title) (s-match (rx "<title>" (group (* nonl)) "</title>") html))
         ((_ charset) (-map 'intern (s-match (rx "charset=" (group (+ (any "-" alnum)))) html))))
    (cb-org-capture-url--decode-html-entities (if (-contains? coding-system-list charset)
                                (decode-coding-string title charset)
                              title))))

(defun cb-org-capture-url--retrieve-html (url)
  "Download the resource at URL and attempt to extract an HTML title."
  (unless (s-matches? (rx "." (or "pdf" "mov" "mp4" "m4v" "aiff" "wav" "mp3") eol) url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (buffer-string))))

(defun cb-org-capture-url--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (cons (current-kill 0 t) kill-ring)))

(defun cb-org-capture-url--read-string-with-default (prompt default &optional initial-input history)
  (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
               initial-input history default))

(defun cb-org-capture-url-read-url ()
  "Return a URL capture template string for use with `org-capture'."
  (let* ((default (or (thing-at-point-url-at-point) (cb-org-capture-url--last-url-kill)))
         (url (cb-org-capture-url--read-string-with-default "URL" default))
         (title (cb-org-capture-url--parse-html-title (cb-org-capture-url--retrieve-html url))))
    (format "* [[%s][%s]]" url (or title url))))

(provide 'cb-org-capture-url)

;;; cb-org-capture-url.el ends here
