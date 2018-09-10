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


;;; JQL query utils

(defun jql-date-operand-to-time (x)
  (pcase x
    ('today
     (current-time))
    (x
     (or
      (ignore-errors
        (org-time-string-to-time (format "%s" x)))

      (pcase (s-match (rx bos (group (+ digit)) (group (or "w" "d")) eos) (format "%s" x))
        (`(,_ ,(app string-to-number count) "d")
         (days-to-time count))
        (`(,_ ,(app string-to-number count) "w")
         (days-to-time (* 7 count)))
        (_
         (error "Parse error for time value: %s" x)))))))

(defun jql-eval-date (expr)
  (pcase expr
    ('today
     (format-time-string "%F"))
    (`(+ ,l ,r)
     (format-time-string "%F" (time-add (jql-date-operand-to-time l)
                                        (jql-date-operand-to-time r))))
    (`(- ,l ,r)
     (format-time-string "%F" (time-subtract (jql-date-operand-to-time l)
                                             (jql-date-operand-to-time r))))
    (_
     (pcase (s-match (rx bos (group (or "-" "+")) (group (+ nonl))) (format "%s" expr))
       (`(,_ "-" ,time)
        (format-time-string "%F" (time-subtract nil (jql-date-operand-to-time time))))
       (`(,_ "+" ,time)
        (format-time-string "%F" (time-add nil (jql-date-operand-to-time time))))
       (_
        (or (ignore-errors
              (format-time-string "%F" (org-time-string-to-time (format "%s" expr))))
            expr))))))

(defun jql-eval (expr &optional depth)
  (let ((depth (or depth 0)))
    (pcase expr
      ((pred numberp)
       (number-to-string expr))
      ((or (pred symbolp) (pred stringp))
       (prin1-to-string expr))
      (`(and . ,xs)
       (pcase (seq-remove #'string-empty-p (--map (jql-eval it (1+ depth)) xs))
         ('() "")
         (`(,clause)
          (format "%s" clause))
         ((and clauses (guard (zerop depth)))
          (format "%s" (string-join clauses " AND ")))
         (clauses
          (format "(%s)" (string-join clauses " AND ")))))
      (`(or . ,xs)
       (pcase (seq-remove #'string-empty-p (--map (jql-eval it (1+ depth)) xs))
         ('() "")
         (`(,clause)
          (format "%s" clause))
         ((and clauses (guard (zerop depth)))
          (format "%s" (string-join clauses " OR ")))
         (clauses
          (format "(%s)" (string-join clauses " OR ")))))
      (`(labels)
       "")
      (`(labels ,x)
       (format "labels = %s" (prin1-to-string x)))
      (`(labels . ,xs)
       (format "labels in (%s)" (string-join (seq-map #'prin1-to-string xs) ", ")))
      (`(eval ,expr)
       (jql-eval (eval expr) (1+ depth)))

      ((and `(,role ,user)
            (guard (seq-contains '(assignee creator) role))
            (guard (or (stringp user) (symbolp user))))
       (format "%s = %s" role (prin1-to-string user)))

      (`(created ,date-expr)
       (format "created = %s" (jql-eval-date date-expr)))
      ((and `(created ,op ,time)
            (guard (seq-contains '(> >= < <= = before after) op)))
       (format "created %s %s"
               (pcase op
                 ('before "<")
                 ('after ">")
                 (op op))
               (jql-eval-date time)))

      (`(project ,x)
       (format "project = %s" (prin1-to-string x)))
      (`(epic ,x)
       (format "\"Epic Link\" = %s" (prin1-to-string x)))

      (_
       (error "JQL parse failure: %s" expr)))))

(cl-assert (equal (jql-eval '(and)) ""))
(cl-assert (equal (jql-eval 1) "1"))
(cl-assert (equal (jql-eval "hi") "\"hi\""))
(cl-assert (equal (jql-eval 'hi) "hi"))
(cl-assert (equal (jql-eval '(and)) ""))
(cl-assert (equal (jql-eval '(and 1)) "1"))
(cl-assert (equal (jql-eval '(and 1 2)) "1 AND 2"))
(cl-assert (equal (jql-eval '(and 1 2 3)) "1 AND 2 AND 3"))
(cl-assert (equal (jql-eval '(or)) ""))
(cl-assert (equal (jql-eval '(or 1)) "1"))
(cl-assert (equal (jql-eval '(or 1 2)) "1 OR 2"))
(cl-assert (equal (jql-eval '(or 1 2 3)) "1 OR 2 OR 3"))
(cl-assert (equal (jql-eval '(or 1 (and 2 3))) "1 OR (2 AND 3)"))
(cl-assert (equal (jql-eval '(eval (+ 1 2))) "3"))
(cl-assert (equal (jql-eval '(creator "foo")) "creator = \"foo\""))
(cl-assert (equal (jql-eval '(assignee "foo")) "assignee = \"foo\""))
(cl-assert (equal (jql-eval '(created today)) (format-time-string "created = %F")))

(cl-assert (equal (jql-eval '(created = 2018-01-01)) "created = 2018-01-01"))
(cl-assert (equal (jql-eval '(created < 2018-01-01)) "created < 2018-01-01"))
(cl-assert (equal (jql-eval '(created <= 2018-01-01)) "created <= 2018-01-01"))
(cl-assert (equal (jql-eval '(created > 2018-01-01)) "created > 2018-01-01"))
(cl-assert (equal (jql-eval '(created >= 2018-01-01)) "created >= 2018-01-01"))
(cl-assert (equal (jql-eval '(created before 2018-01-01)) "created < 2018-01-01"))
(cl-assert (equal (jql-eval '(created after 2018-01-01)) "created > 2018-01-01"))

(cl-assert (equal (jql-eval '(created before "2018-01-01")) "created < 2018-01-01"))
(cl-assert (equal (jql-eval '(created after "2018-01-01")) "created > 2018-01-01"))

(cl-assert (equal (jql-eval '(created before "[2018-01-01]")) "created < 2018-01-01"))
(cl-assert (equal (jql-eval '(created after "[2018-01-01]")) "created > 2018-01-01"))

(cl-assert (equal (jql-eval '(created after (+ 1d 2018-01-01))) "created > 2018-01-02"))
(cl-assert (equal (jql-eval '(created after (+ 2018-01-01 1d))) "created > 2018-01-02"))
(cl-assert (equal (jql-eval '(created after (+ 2d 2018-01-01))) "created > 2018-01-03"))
(cl-assert (equal (jql-eval '(created after (+ 2018-01-01 2d))) "created > 2018-01-03"))
(cl-assert (equal (jql-eval '(created after (+ 2018-01-01 1w))) "created > 2018-01-08"))

(cl-assert (equal (jql-eval '(created after (- 2018-01-04 1d))) "created > 2018-01-03"))
(cl-assert (equal (jql-eval '(created after (- 2018-01-04 2d))) "created > 2018-01-02"))

(cl-assert (equal (jql-eval '(created after (- 2018-01-08 1w))) "created > 2018-01-01"))

(cl-assert (equal (jql-eval '(created after -1w)) (format-time-string "created > %F" (time-subtract nil (days-to-time 7)))))
(cl-assert (equal (jql-eval '(created after +1w)) (format-time-string "created > %F" (time-add nil (days-to-time 7)))))

(cl-assert (equal (jql-eval '(labels)) ""))
(cl-assert (equal (jql-eval '(labels foo)) "labels = foo"))
(cl-assert (equal (jql-eval '(labels foo bar baz)) "labels in (foo, bar, baz)"))
(cl-assert (equal (jql-eval '(project foo)) "project = foo"))
(cl-assert (equal (jql-eval '(epic foo)) "\"Epic Link\" = foo"))

(defun jira-utils-search-issues (jql-expr)
  (let* ((jql-param (url-hexify-string (jql-eval jql-expr)))
         (url (concat jira-utils-url-base "/issues/?jql=" jql-param)))
    (browse-url url)))

(defun jira-utils-search-issues-with-jql (&rest jql)
  (let* ((jql-param (url-hexify-string (string-join jql " ")))
         (url (concat jira-utils-url-base "/issues/?jql=" jql-param)))
    (browse-url url)))

(provide 'jira-utils)

;;; jira-utils.el ends here
