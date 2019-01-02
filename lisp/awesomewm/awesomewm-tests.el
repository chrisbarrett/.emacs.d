;;; awesomewm-tests.el --- Tests for awesomewm client lib  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'awesomewm)
(require 'ert)

(defun awesomewm--trim (str)
  (let ((sans-prefixes (s-replace-regexp (rx bol (* space) "|") "" str)))
    (string-trim sans-prefixes)))

(ert-deftest awesomewm--compile-string ()
  (should (equal "\"hello\"" (awesomewm-compile "hello")))
  (should (equal "\"hello\\nworld\"" (awesomewm-compile "hello\nworld"))))

(ert-deftest awesomewm--compile-symbol ()
  (should (equal "foo" (awesomewm-compile 'foo)))
  (should (equal "foo_bar_baz" (awesomewm-compile 'foo-bar-baz))))

(ert-deftest awesomewm--compile-member-access ()
  (should (equal "foo.bar" (awesomewm-compile 'foo.bar))))

(ert-deftest awesomewm--compile-hash ()
  (should (equal "{1}"
                 (awesomewm-compile '[1])))
  (should (equal "{foo = 1}"
                 (awesomewm-compile '[(foo . 1)])))
  (should (equal "{foo_bar = 1}"
                 (awesomewm-compile '[(foo-bar . 1)])))
  (should (equal "{foo = 1, bar}"
                 (awesomewm-compile '[(foo . 1) bar])))
  (should (equal "{foo = 1, bar = 2}"
                 (awesomewm-compile '[(foo . 1)
                             (bar . 2)])))
  (should (equal "{baz = {bar = 2}}"
                 (awesomewm-compile '[(baz . [(bar . 2)])]))))

(ert-deftest awesomewm--compile-funcall ()
  (should (equal "foo()" (awesomewm-compile '(foo))))
  (should (equal "foo(bar)" (awesomewm-compile '(foo bar))))
  (should (equal "foo(bar, baz)" (awesomewm-compile '(foo bar baz))))
  (should (equal "foo(bar(baz))" (awesomewm-compile '(foo (bar baz)))))
  (should (equal "foo(bar, bar(baz))" (awesomewm-compile '(foo bar (bar baz))))))

(ert-deftest awesomewm--compile-let ()
  (should (equal "local x = 1" (awesomewm-compile '(let ((x 1))))))
  (should (equal "local x_y = 1" (awesomewm-compile '(let ((x-y 1))))))
  (should (equal (awesomewm--trim "
                  |
                  |local x = 1
                  |x
                  |")
                 (awesomewm-compile '(let ((x 1))
                              x)))))

(ert-deftest awesomewm--program-with-let ()
  (should (equal (awesomewm--trim "
                  |
                  |local a = require(\"module\")
                  |a.b({c = 1, d = 2})
                  |
                  |")
                 (awesomewm-compile
                  `(let ((a (require "module")))
                     (a.b [(c . 1)
                           (d . 2)]))))))

(provide 'awesomewm-tests)

;;; awesomewm-tests.el ends here
