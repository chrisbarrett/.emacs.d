;;; cb-org-capture-url.el --- Utilities for capturing URLs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'org-funcs)
(require 's)

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

(defun cb-org-capture-url-read-url ()
  "Return a URL capture template string for use with `org-capture'."
  (let* ((url (org-funcs-read-url "URL"))
         (title (cb-org-capture-url--parse-html-title (cb-org-capture-url--retrieve-html url))))
    (format "* TODO Review [[%s][%s]]" url (or title url))))

(provide 'cb-org-capture-url)

;;; cb-org-capture-url.el ends here
