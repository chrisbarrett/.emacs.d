;;; js-lex-test.el --- Tests for js-lex.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 's)


(defsubst js-lex-test--erase-positions (tokens)
  (--map (-take 2 it) tokens))

(defmacro js-lex-test-spec-to-ert (spec-file)
  (let ((test-name (intern (format "js-lex--test--%s" (file-name-base spec-file)))))
    `(ert-deftest ,test-name ()
       (-let* ((str (f-read ,spec-file))
               ((_ part-1 part-2)
                (s-match (rx (group-n 1 (*? anything))
                             (>= 4 "-")
                             (group-n 2 (* anything)))
                         str))
               (_ (should part-1))
               (_ (should part-2))
               (input part-1)
               (actual (js-lex-test--erase-positions (js-lex input)))
               (expected (read part-2)))
         (should (equal expected
                        actual))))))


;; Create ert tests from spec files.

(eval-when-compile
  (defconst this-dir
    (if load-in-progress
        (file-name-directory load-file-name)
      default-directory)))

(require 'js-lex (f-join (f-parent this-dir) "js-lex.el"))


(let* ((spec-dir (f-join this-dir "specs"))
       (spec-files (f-files spec-dir (lambda (it) (f-ext? it "txt")))))
  (dolist (file spec-files)
    (eval `(js-lex-test-spec-to-ert ,file))))

(provide 'js-lex-test)

;;; js-lex-test.el ends here
