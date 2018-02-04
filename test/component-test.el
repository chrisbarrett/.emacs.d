;;; component-test.el --- Tests for component.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (bound-and-true-p test-helper-loaded)
    (load (concat default-directory "test-helper.el"))))

(require 'component)
(require 'magit-section)
(require 'subr-x)


;; rejection

(ert-deftest component-test--rejects-invalid-ast ()
  (let* ((props '((message . ignore)))
         (ast [hello]))
    (with-temp-buffer
      (should-error (component-eval ast nil props)))))

(ert-deftest component-test--rejects-undefined-component ()
  (let ((ast '(foo "bar")))
    (with-temp-buffer
      (should-error (component-eval ast)))))


;; padding

(ert-deftest component-test--inserts-newline-for-padding ()
  (let ((ast '(padding)))
    (with-temp-buffer
      (component-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest component-test--padding-errors-if-applied-to-arguments ()
  (let ((ast '(padding foo)))
    (with-temp-buffer
      (should-error (component-eval ast)))))


;; line

(ert-deftest component-test--inserts-newlines-for-empty-strings ()
  (let ((ast '(line "")))
    (with-temp-buffer
      (component-eval ast)
      (should (equal "\n" (buffer-string))))))

(ert-deftest component-test--inserts-strings ()
  (let ((ast '(line "foo")))
    (with-temp-buffer
      (component-eval ast)
      (should (equal "foo\n" (buffer-string))))))


;; list

(ert-deftest component-test--lists ()
  (let ((ast '(list (line "foo") (line "bar") (line "baz")))
        (expected (string-trim-left "
- foo
- bar
- baz
")))
    (with-temp-buffer
      (component-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest component-test--lists-with-multiple-lines ()
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
      (component-eval ast)
      (should (equal expected (buffer-string))))))

(ert-deftest component-test--lists-with-indentation ()
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
      (component-eval ast)
      (should (equal expected (buffer-string))))))


;; propertize

(ert-deftest component-test--propertizes-regions ()
  (let ((ast '(propertize (face error) (line "foo"))))
    (with-temp-buffer
      (component-eval ast)
      (should (equal (propertize "foo\n" 'face 'error) (buffer-string))))))


;; sequence

(ert-deftest component-test--sequencing-actions ()
  (let ((ast '((line "foo")
               (line "bar"))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "foo\nbar\n" (buffer-string))))))


;; heading

(ert-deftest component-test--inserts-headings ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (save-excursion
        (magit-insert-section (test)
          (component-eval ast)))
      (should (equal "hello\n" (substring-no-properties (buffer-string))))
      (should (equal 'magit-section-heading (face-at-point))))))

(ert-deftest component-test--inserting-heading-raises-error-outside-section ()
  (let ((ast '(heading "hello")))
    (with-temp-buffer
      (should-error (component-eval ast)))))


;; section

(ert-deftest component-test--inserting-sections ()
  (let ((ast '(section (root nil)
                       (section (test-line nil)
                                (line "foo")))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "foo\n" (substring-no-properties (buffer-string))))
      (should (magit-current-section))
      (should (equal 'test-line (magit-section-type (magit-current-section))))
      (should (not (magit-section-hidden (magit-current-section)))))))


;; indent

(ert-deftest component-test--indents-ast ()
  (let ((ast '(indent (line "hello"))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "  hello\n" (substring-no-properties (buffer-string)))))))

(ert-deftest component-test--indentation-padding-lacks-properties-directly-set-on-string ()
  (let ((ast `(indent (line ,(propertize "hello" 'face 'font-lock-warning-face)))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (not (text-property-any 0 component--indentation-width
                                      'face 'font-lock-warning-face
                                      (buffer-string)))))))

(ert-deftest component-test--indentation-padding-has-ast-declared-properties ()
  (let ((ast '(indent
               (propertize (face font-lock-warning-face)
                           (line "hello")))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "  " (substring-no-properties (buffer-string) 0 2)))
      (should (text-property-any 0 component--indentation-width
                                 'face 'font-lock-warning-face
                                 (buffer-string))))))


;; key-value

(ert-deftest component-test--key-value-pairs ()
  (let ((ast '(key-value 10 "Key" "Value")))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal (format "%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))

(ert-deftest component-test--key-value--validates-width-as-nat ()
  (with-temp-buffer
    (should-assert (component-eval '(key-value "10" "Key" "Value")))
    (should-assert (component-eval '(key-value -1 "Key" "Value")))))

(ert-deftest component-test--key-value--validates-key-as-string ()
  (with-temp-buffer
    (should-assert (component-eval '(key-value 10 nil "Value")))))

(ert-deftest component-test--key-value--empty-output-if-value-is-nil ()
  (with-temp-buffer
    (component-eval '(key-value 10 "Key" nil))
    (should (string-empty-p (buffer-string)))))

(ert-deftest component-test--key-value--ensures-values-are-inserted-on-new-lines ()
  (let ((ast '("foo" (key-value 10 "Key" "Value"))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal (format "foo\n%-10s%s\n" "Key:" "Value") (substring-no-properties (buffer-string)))))))


;; copy-prop

(ert-deftest component-test--copy-prop ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (should (equal "Test\n" (buffer-string)))
      (should (equal (list 'component-copy "foo")
                     (text-properties-at (point-min) (buffer-string)))))))

(ert-deftest component-test--copy-prop-finishes-at-end-of-line ()
  (let ((ast '(copy-prop "foo" (line "Test"))))
    (with-temp-buffer
      (save-excursion (component-eval ast))
      (let ((end (1- (line-end-position))))
        (should (equal (list 'component-copy "foo")
                       (text-properties-at end (buffer-string))))))))


;; Test rendering function's point restoration.

(ert-deftest component-test--render--no-err-if-empty-buffer ()
  (with-temp-buffer
    (component-render (current-buffer) nil)
    (should (string-empty-p (buffer-string)))))

(ert-deftest component-test--render--restores-point--1 ()
  (let ((ast nil))
    (with-temp-buffer
      (component-render (current-buffer) ast))))

(ert-deftest component-test--render--restores-point--2 ()
  (let ((expected-column) (expected-line)
        (ast '(line "foo")))
    (with-temp-buffer

      (component-render (current-buffer) ast)
      (goto-char (point-min))
      (search-forward "o")

      (setq expected-column (current-column))
      (setq expected-line (line-number-at-pos))

      (component-render (current-buffer) ast)

      (should (equal (current-column) expected-column))
      (should (equal (line-number-at-pos) expected-line)))))

(ert-deftest component-test--render--restores-point--3 ()
  (let ((expected-column) (expected-line)
        (ast '(section (root)
                       (section (line1) (line "foo"))
                       (section (line2) (line "bar"))
                       (section (line3) (line "baz")))))
    (with-temp-buffer

      (component-render (current-buffer) ast)
      (goto-char (point-min))
      (search-forward "ba")

      (setq expected-column (current-column))
      (setq expected-line (line-number-at-pos))

      (component-render (current-buffer) ast)

      (should (equal (current-column) expected-column))
      (should (equal (line-number-at-pos) expected-line)))))

(defun component-test--line-has-highlight-p ()
  (let ((overlay-faces-at-pt
         (--map (plist-get (overlay-properties it) 'face)
                (overlays-at (point)))))
    (memq 'magit-section-highlight overlay-faces-at-pt)))

(ert-deftest component-test--render--updates-highlight ()
  (let ((ast '(section (root)
                       (section (line1) (line "foo"))
                       (section (line2) (line "bar"))
                       (section (line3) (line "baz")))))
    (with-temp-buffer
      (component-render (current-buffer) ast)
      (should (component-test--line-has-highlight-p))

      (goto-char (point-min))
      (search-forward "ba")
      (component-render (current-buffer) ast)

      (should (component-test--line-has-highlight-p))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (component-test--line-has-highlight-p))))))

(provide 'component-test)

;;; component-test.el ends here
