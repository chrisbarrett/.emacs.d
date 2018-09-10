;;; jira-utils-tests.el --- Tests for jira-utils.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'jira-utils)

(ert-deftest jira-utils-tests--immediates ()
  (should (equal (jql-eval 1) "1"))
  (should (equal (jql-eval "hi") "\"hi\""))
  (should (equal (jql-eval 'hi) "hi")))

(ert-deftest jira-utils-tests--and ()
  (should (equal (jql-eval '(and)) ""))
  (should (equal (jql-eval '(and)) ""))
  (should (equal (jql-eval '(and 1)) "1"))
  (should (equal (jql-eval '(and 1 2)) "1 AND 2"))
  (should (equal (jql-eval '(and 1 2 3)) "1 AND 2 AND 3")))

(ert-deftest jira-utils-tests--or ()
  (should (equal (jql-eval '(or)) ""))
  (should (equal (jql-eval '(or 1)) "1"))
  (should (equal (jql-eval '(or 1 2)) "1 OR 2"))
  (should (equal (jql-eval '(or 1 2 3)) "1 OR 2 OR 3"))
  (should (equal (jql-eval '(or 1 (and 2 3))) "1 OR (2 AND 3)")))

(ert-deftest jira-utils-tests--eval ()
  (should (equal (jql-eval '(eval (+ 1 2))) "3")))

(ert-deftest jira-utils-tests--order ()
  (should (equal (jql-eval '(by foo 1)) "1 ORDER BY foo"))
  (should (equal (jql-eval '(by (foo bar) 1)) "1 ORDER BY foo, bar")))

(ert-deftest jira-utils-tests--attrs ()
  (should (equal (jql-eval '(labels)) ""))
  (should (equal (jql-eval '(labels foo)) "labels = foo"))
  (should (equal (jql-eval '(labels foo bar baz)) "labels in (foo, bar, baz)"))
  (should (equal (jql-eval '(creator "foo")) "creator = \"foo\""))
  (should (equal (jql-eval '(assignee "foo")) "assignee = \"foo\""))
  (should (equal (jql-eval '(project foo)) "project = foo"))
  (should (equal (jql-eval '(filter foo)) "filter = foo"))
  (should (equal (jql-eval '(status foo)) "status = foo")))

(ert-deftest jira-utils-tests--attr-with-custom-name ()
  (should (equal (jql-eval '(epic foo)) "\"Epic Link\" = foo")))

(ert-deftest jira-utils-tests--today ()
  (should (equal (jql-eval '(created today)) (format-time-string "created = %F"))))

(ert-deftest jira-utils-tests--date-comparison ()
  (should (equal (jql-eval '(= created 2018-01-01)) "created = 2018-01-01"))
  (should (equal (jql-eval '(created = 2018-01-01)) "created = 2018-01-01"))
  (should (equal (jql-eval '(created < 2018-01-01)) "created < 2018-01-01"))
  (should (equal (jql-eval '(created <= 2018-01-01)) "created <= 2018-01-01"))
  (should (equal (jql-eval '(created > 2018-01-01)) "created > 2018-01-01"))
  (should (equal (jql-eval '(created >= 2018-01-01)) "created >= 2018-01-01"))
  (should (equal (jql-eval '(created before 2018-01-01)) "created < 2018-01-01"))
  (should (equal (jql-eval '(created after 2018-01-01)) "created > 2018-01-01"))
  (should (equal (jql-eval '(created before "2018-01-01")) "created < 2018-01-01"))
  (should (equal (jql-eval '(created after "2018-01-01")) "created > 2018-01-01"))
  (should (equal (jql-eval '(created before "[2018-01-01]")) "created < 2018-01-01"))
  (should (equal (jql-eval '(created after "[2018-01-01]")) "created > 2018-01-01")))

(ert-deftest jira-utils-tests--created-between ()
  (should (equal (jql-eval '(created between 2018-01-01 2018-01-03)) "created >= 2018-01-01 AND created <= 2018-01-03")))

(ert-deftest jira-utils-tests--date-arithmetic ()
  (should (equal (jql-eval '(created after (+ 1d 2018-01-01))) "created > 2018-01-02"))
  (should (equal (jql-eval '(created after (+ 2018-01-01 1d))) "created > 2018-01-02"))
  (should (equal (jql-eval '(created after (+ 2d 2018-01-01))) "created > 2018-01-03"))
  (should (equal (jql-eval '(created after (+ 2018-01-01 2d))) "created > 2018-01-03"))
  (should (equal (jql-eval '(created after (+ 2018-01-01 1w))) "created > 2018-01-08"))
  (should (equal (jql-eval '(created after (2018-01-01 + 1w))) "created > 2018-01-08"))
  (should (equal (jql-eval '(created after (- 2018-01-04 1d))) "created > 2018-01-03"))
  (should (equal (jql-eval '(created after (- 2018-01-04 2d))) "created > 2018-01-02"))
  (should (equal (jql-eval '(created after (- 2018-01-08 1w))) "created > 2018-01-01"))
  (should (equal (jql-eval '(created after (2018-01-08 - 1w))) "created > 2018-01-01")))

(ert-deftest jira-utils-tests--date-arithmetic-relative-to-today ()
  (should (equal (jql-eval '(created after -1w)) (format-time-string "created > %F" (time-subtract nil (days-to-time 7)))))
  (should (equal (jql-eval '(created after +1w)) (format-time-string "created > %F" (time-add nil (days-to-time 7))))))


(provide 'jira-utils-tests)

;;; jira-utils-tests.el ends here
