;;; blergh-test.el --- Tests for blergh.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'blergh)

(defmacro blergh--should-assert (form)
  `(let ((debug-on-error nil))
     (should-error ,form :type 'cl-assertion-failed)))


;; rejection

(ert-deftest blergh-test--rejects-invalid-ast ()
  (let ((ast '(foo "bar")))
    (with-temp-buffer
      (should-error (blergh-eval ast)))))


;; padding

(ert-deftest blergh-test--inserts-newline-for-padding ()
  (let ((ast '(padding)))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest blergh-test--padding-errors-if-applied-to-arguments ()
  (let ((ast '(padding foo)))
    (with-temp-buffer
      (should-error (blergh-eval ast)))))


;; line

(ert-deftest blergh-test--inserts-newlines-for-empty-strings ()
  (let ((ast '(line "")))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest blergh-test--inserts-strings ()
  (let ((ast '(line "foo")))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal "foo\n" (buffer-string))))))


;; list

(ert-deftest blergh-test--lists ()
  (let ((ast '(list (line "foo") (line "bar") (line "baz")))
        (expected (string-trim-left "
- foo
- bar
- baz
")))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest blergh-test--lists-with-multiple-lines ()
  (let ((ast '(list ((line "foo") (line "bar"))
                    ((line "foo") (line "  bar"))
                    ("foo" "bar")))
        (expected (string-trim-left "
- foo
  bar
- foo
    bar
- foobar")))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest blergh-test--lists-with-indentation ()
  (let ((ast '(list (line "foo")
                    (line "bar")
                    (list (line "foo")
                          (line "bar"))
                    (line "baz")))
        (expected (string-trim-left "
- foo
- bar
  - foo
  - bar
- baz
")))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal expected (buffer-string))))))


;; propertize

(ert-deftest blergh-test--propertizes-regions ()
  (let ((ast '(propertize (face error) (line "foo"))))
    (with-temp-buffer
      (blergh-eval ast)
      (should (equal (propertize "foo\n" 'face 'error) (buffer-string))))))


;; sequence

(ert-deftest blergh-test--sequencing-actions ()
  (let ((ast '((line "foo")
               (line "bar"))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal "foo\nbar\n" (buffer-string))))))


;; heading

(ert-deftest blergh-test--inserts-headings ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (save-excursion
        (magit-insert-section (test)
          (blergh-eval ast)))
      (should (equal "hello\n" (substring-no-properties (buffer-string))))
      (should (equal 'magit-section-heading (face-at-point))))))

(ert-deftest blergh-test--inserting-heading-raises-error-outside-section ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (should-error (blergh-eval ast)))))


;; section

(ert-deftest blergh-test--inserting-sections ()
  (let ((ast '(section (test nil)
                       (line "foo"))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (magit-current-section))
      (should (equal 'test (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))


;; indent

(ert-deftest blergh-test--indents-ast ()
  (let ((ast '(indent (line "hello"))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal "  hello\n" (substring-no-properties (buffer-string)))))))

(ert-deftest blergh-test--indentation-padding-lacks-properties-directly-set-on-string ()
  (let ((ast `(indent (line ,(propertize "hello" 'face 'font-lock-warning-face)))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (not (text-property-any 0 blergh--indentation-width
                                      'face 'font-lock-warning-face
                                      (buffer-string)))))))

(ert-deftest blergh-test--indentation-padding-has-ast-declared-properties ()
  (let ((ast '(indent
               (propertize (face font-lock-warning-face)
                           (line "hello")))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (text-property-any 0 blergh--indentation-width
                                 'face 'font-lock-warning-face
                                 (buffer-string))))))


;; key-value

(ert-deftest blergh-test--key-value-pairs ()
  (let ((ast '(key-value 10 "Key" "Value")))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal (format "%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))

(ert-deftest blergh-test--key-value--validates-width-as-nat ()
  (with-temp-buffer
    (blergh--should-assert (blergh-eval '(key-value "10" "Key" "Value")))
    (blergh--should-assert (blergh-eval '(key-value -1 "Key" "Value")))))

(ert-deftest blergh-test--key-value--validates-key-as-string ()
  (with-temp-buffer
    (blergh--should-assert (blergh-eval '(key-value 10 nil "Value")))))

(ert-deftest blergh-test--key-value--validates-value-as-string ()
  (with-temp-buffer
    (blergh--should-assert (blergh-eval '(key-value 10 "Key" 1)))))

(ert-deftest blergh-test--key-value--ensures-values-are-inserted-on-new-lines ()
  (let ((ast '("foo" (key-value 10 "Key" "Value"))))
    (with-temp-buffer
      (save-excursion (blergh-eval ast))
      (should (equal (format "foo\n%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))


(provide 'blergh-test)

;;; blergh-test.el ends here
