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

(ert-deftest jira-utils-tests--not ()
  (should (equal (jql-eval '(not 1)) "NOT 1"))
  (should (equal (jql-eval '(not (not 1))) "NOT (NOT 1)"))
  (should-error (jql-eval '(not)))
  (should-error (jql-eval '(not 1 2))))

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
  (should (equal (jql-eval '(labels foo bar baz)) "labels IN (foo, bar, baz)"))
  (should (equal (jql-eval '(creator "foo")) "creator = \"foo\""))
  (should (equal (jql-eval '(assignee "foo")) "assignee = \"foo\""))
  (should (equal (jql-eval '(project foo)) "project = foo"))
  (should (equal (jql-eval '(project != foo)) "project != foo"))
  (should (equal (jql-eval '(project != foo bar)) "project NOT IN (foo, bar)"))
  (should (equal (jql-eval '(filter foo)) "filter = foo"))
  (should (equal (jql-eval '(status foo)) "status = foo"))
  (should (equal (jql-eval '(resolution is empty)) "resolution IS EMPTY")))

(ert-deftest jira-utils-tests--attr-with-custom-name ()
  (should (equal (jql-eval '(epic foo)) "\"Epic Link\" = foo")))

(ert-deftest jira-utils-tests--today ()
  (should (equal (jql-eval '(created today)) (format-time-string "created = %F"))))

(ert-deftest jira-utils-tests--status-was ()
  (should (equal (jql-eval '(status was "foo" on 2018-01-01)) "status WAS foo ON 2018-01-01"))
  (should (equal (jql-eval '(status was empty on 2018-01-01)) "status WAS EMPTY ON 2018-01-01"))
  (should (equal (jql-eval '(status was not "foo" on 2018-01-01)) "status WAS NOT foo ON 2018-01-01"))
  (should (equal (jql-eval '(status was not empty on 2018-01-01)) "status WAS NOT EMPTY ON 2018-01-01"))
  (should (equal (jql-eval '(status was "foo" before 2018-01-01)) "status WAS foo BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status was (foo bar) before 2018-01-01)) "status WAS IN (foo, bar) BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status was not (foo bar) before 2018-01-01)) "status WAS NOT IN (foo, bar) BEFORE 2018-01-01"))
  (should-error (jql-eval '(status was "foo"))))

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

(ert-deftest jira-utils-tests--status-changed ()
  (should (equal (jql-eval '(status-changed 2018-01-01)) "status changed ON 2018-01-01"))
  (should (equal (jql-eval '(status-changed on 2018-01-01)) "status changed ON 2018-01-01"))
  (should (equal (jql-eval '(status-changed before 2018-01-01)) "status changed BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status-changed after 2018-01-01)) "status changed AFTER 2018-01-01"))
  (should (equal (jql-eval '(status-changed before "2018-01-01")) "status changed BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status-changed after "2018-01-01")) "status changed AFTER 2018-01-01"))
  (should (equal (jql-eval '(status-changed before "[2018-01-01]")) "status changed BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status-changed after "[2018-01-01]")) "status changed AFTER 2018-01-01"))
  (should (equal (jql-eval '(status-changed between 2018-01-01 2018-01-03)) "status changed AFTER 2018-01-01 AND status changed BEFORE 2018-01-03")))

(ert-deftest jira-utils-tests--status-changed-to ()
  (should (equal (jql-eval '(status-changed to "In Progress" on 2018-01-01)) "status changed TO \"In Progress\" ON 2018-01-01"))
  (should (equal (jql-eval '(status-changed to "foo" before 2018-01-01)) "status changed TO \"foo\" BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status-changed before 2018-01-01 to "foo" )) "status changed TO \"foo\" BEFORE 2018-01-01"))
  (should (equal (jql-eval '(status-changed to "bar" after 2018-01-01)) "status changed TO \"bar\" AFTER 2018-01-01")))

(ert-deftest jira-utils-tests--created-between ()
  (should (equal (jql-eval '(created between 2018-01-01 2018-01-03)) "created >= 2018-01-01 AND created <= 2018-01-03"))
  (should (equal (jql-eval '(created between 2018-01-01 and 2018-01-03)) "created >= 2018-01-01 AND created <= 2018-01-03")))

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

(ert-deftest jira-utils-tests--date-preceding-days ()
  (should (jql-eval '(created after -thu))))

(ert-deftest jira-utils-tests--date-arithmetic-relative-to-today ()
  (should (equal (jql-eval '(created after -1w)) (format-time-string "created > %F" (time-subtract nil (days-to-time 7)))))
  (should (equal (jql-eval '(created after +1w)) (format-time-string "created > %F" (time-add nil (days-to-time 7))))))


(provide 'jira-utils-tests)

;;; jira-utils-tests.el ends here
