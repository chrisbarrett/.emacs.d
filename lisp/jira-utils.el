;;; jira-utils.el --- Utils for working with Jira. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cb-org-capture-url)
(require 'dash)
(require 'f)
(require 'xml)
(require 'subr-x)

(defvar jira-utils-url-base nil
  "The base URL of the Jira installation to query.
e.g. https://example.atlassian.net/")

(defvar jira-utils-cookie-file "~/etc/jira_cookie.txt.gpg"
  "Path to a gpg-encrypted file containing a non-expired Jira API token.")

(defun jira-utils--cookie ()
  (let ((cipher (f-read jira-utils-cookie-file)))
    (s-trim (epg-decrypt-string (epg-make-context) cipher))))

(defun jira-utils--issue-url (issue-number)
  (format "%s/browse/%s" jira-utils-url-base issue-number))

(defun jira-utils--issue-number-or-url-from-kill-ring ()
  (--first (s-matches? (rx-to-string `(and bos
                                           (? (and ,jira-utils-url-base "/browse/"))
                                           (+ alpha) "-" (+ digit)
                                           (? "?" (* nonl))
                                           eos))
                       it)
           (cons (current-kill 0 t) kill-ring)))

(defun jira-utils--xml-node-with-id (id xml)
  (cl-labels ((loop (xml)
                    (when (listp xml)
                      (if (equal id (xml-get-attribute xml 'id))
                          (throw 'result xml)
                        (dolist (child (xml-node-children xml))
                          (loop child))))))
    (catch 'result
      (loop xml))))

(defun jira-utils--get-html (url)
  (let ((url-request-extra-headers `(("cookie" . ,(jira-utils--cookie)))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (search-forward "<!DOCTYPE html>")
      (libxml-parse-html-region
       (line-beginning-position)
       (point-max)))))

(defun jira-utils--parse-html-title (raw-title)
  (cdr (s-match (rx bos "[" (group (+? nonl)) "]" (+ space) (group (+? nonl))
                    ;; Omit JIRA workspace from title
                    (+ space) "-" (+ (not (any "-")))
                    eos)
                raw-title)))

(defun jira-utils-format-org-capture-reference (issue url title assignee)
  (concat (format "- [[%s][%s]] (/%s/) :: %s" url issue (or assignee "unassigned") title)))

(defun jira-utils--parse-assignee (xml)
  (ignore-errors
    (->> (xml-get-children xml 'span)
         (-mapcat 'xml-node-children)
         (-last-item)
         (string-trim))))

(defun jira-utils-read-issue-url-for-org-header ()
  "Return a URL capture template string for use with `org-capture'."
  (interactive)
  (-let* ((url (or (jira-utils--issue-number-or-url-from-kill-ring)
                   (jira-utils--issue-url (read-string "Issue number: "))))
          (html (jira-utils--get-html url))
          ((head body) (xml-node-children html))
          ((_ raw-title) (alist-get 'title (xml-node-children head)))
          (assignee (jira-utils--parse-assignee (jira-utils--xml-node-with-id "assignee-val" body)))
          ((issue title) (jira-utils--parse-html-title raw-title)))
    (jira-utils-format-org-capture-reference issue url title assignee)))

(provide 'jira-utils)

;;; jira-utils.el ends here
