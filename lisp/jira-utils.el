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


;;; Utilities to support directly capturing tickets with org

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
  (concat (format "- [[%s][%s]]%s :: %s"
                  url issue
                  (if assignee (format " (/%s/)" assignee) "")
                  title)))

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

(autoload 'org-time-string-to-time "org")
(autoload 'org-read-date "org")


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
    ((or `(+ ,l ,r) `(,l + ,r))
     (format-time-string "%F" (time-add (jql-date-operand-to-time l)
                                        (jql-date-operand-to-time r))))
    ((or `(- ,l ,r) `(,l - ,r))
     (format-time-string "%F" (time-subtract (jql-date-operand-to-time l)
                                             (jql-date-operand-to-time r))))
    ((guard (string-match-p (rx bos (? "-") (or "mon" "tue" "wed" "thu" "fri" "sat" "sun"))
                            (format "%s" expr)))
     ;; KLUDGE: Just use orgmode to get dates for now.
     (let ((org-rel-date (substring (s-prepend "-" (s-chop-prefix "-" (format "%s" expr))) 0 4)))
       (org-read-date nil nil org-rel-date)))
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

(defconst jira-utils--scalar-attrs
  '((labels)
    (project)
    (filter)
    (status)
    (assignee)
    (epic . "Epic Link")
    (resolution)
    (creator))
  "Alist of DSL attribute names to JQL attribute name.")

(defconst jira-utils--date-attrs
  '((resolution-date . "resolutiondate")
    (resolved)
    (created))
  "Alist of DSL attribute names to JQL attribute name.")

(defconst jira-utils--date-comparators
  '((>)
    (>=)
    (<)
    (<=)
    (=)
    (before . "<")
    (after . ">")
    (since . ">="))
  "Alist of binary date operators to JQL operators name.")

(defun jql-eval (expr &optional depth already-ordered)
  (let ((depth (or depth 0)))
    (pcase expr
      (`(eval ,expr)
       (jql-eval (eval expr) (1+ depth)))

      ;; Immediate values

      ((pred numberp)
       (number-to-string expr))
      ((or (pred symbolp) (pred stringp))
       (prin1-to-string expr))

      ;; Booleans

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

      ;; Scalar attributes

      ((and `(,attr . ,xs)
            (guard (assoc attr jira-utils--scalar-attrs)))
       (let* ((attr-name (format "%s" (or (alist-get attr jira-utils--scalar-attrs) attr)))
              (jql-attr (if (string-match-p " " attr-name) (prin1-to-string attr-name) attr-name)))
         (pcase xs
           (`()
            "")
           (`(is empty)
            (format "%s IS EMPTY" jql-attr))
           (`(,x)
            (format "%s = %s" jql-attr (prin1-to-string x)))
           (`(!= ,x)
            (format "%s != %s" jql-attr (prin1-to-string x)))
           (`(!= . ,xs)
            (format "%s NOT IN (%s)" jql-attr (string-join (seq-map #'prin1-to-string xs) ", ")))
           (xs
            (format "%s IN (%s)" jql-attr (string-join (seq-map #'prin1-to-string xs) ", "))))))

      ;; Date arithmetic

      ((and `(,attr ,expr) (guard (assoc attr jira-utils--date-attrs)))
       (let* ((attr-name (format "%s" (or (alist-get attr jira-utils--date-attrs) attr)))
              (jql-attr (if (string-match-p " " attr-name) (prin1-to-string attr-name) attr-name)))
         (format "%s = %s" jql-attr (jql-eval-date expr))))

      ((and (or `(,op ,attr ,expr)
                `(,attr ,op ,expr))
            (guard (and (assoc attr jira-utils--date-attrs) (assoc op jira-utils--date-comparators))))
       (let* ((attr-name (format "%s" (or (alist-get attr jira-utils--date-attrs) attr)))
              (jql-attr (if (string-match-p " " attr-name) (prin1-to-string attr-name) attr-name))
              (jql-op (or (alist-get op jira-utils--date-comparators) op)))
         (format "%s %s %s" jql-attr jql-op (jql-eval-date expr))))

      ((or `(,attr between ,t1 and ,t2) `(,attr between ,t1 ,t2))
       (jql-eval `(and (,attr >= ,t1) (,attr <= ,t2))
                 depth))

      ;; Order-by clauses

      ((and `(by ,attrs ,expr) (guard (and (listp attrs) (not already-ordered) (zerop depth))))
       (format "%s ORDER BY %s" (jql-eval expr depth t)
               (string-join (--map (format "%s" it) attrs) ", ")))
      ((and `(by ,attr ,expr) (guard (and (not already-ordered) (zerop depth))))
       (format "%s ORDER BY %s" (jql-eval expr depth t) attr))

      (_
       (error "JQL parse failure: %s" expr)))))

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
