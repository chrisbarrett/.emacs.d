;;; company-tern-test.el --- company-tern test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'company-tern)

;;; Annotations.

(ert-deftest test-company-tern-mark-property ()
  (let ((candidate "property"))
    (put-text-property 0 1 'isProperty t candidate)
    (put-text-property 0 1 'depth 0 candidate)
    (put-text-property 0 1 'type "?" candidate)
    (should (s-ends-with-p company-tern-property-marker
                           (company-tern-annotation candidate)))))

(ert-deftest test-company-tern-mark-not-a-property ()
  (let ((candidate "other"))
    (put-text-property 0 1 'isProperty json-false candidate)
    (put-text-property 0 1 'depth 0 candidate)
    (put-text-property 0 1 'type "?" candidate)
    (should-not (s-ends-with-p company-tern-property-marker
                               (company-tern-annotation candidate)))))

(ert-deftest test-company-tern-annotation-null-for-keywords ()
  (let ((candidate "return"))
    (put-text-property 0 1 'isKeyword t candidate)
    (should (null (company-tern-annotation candidate)))))

;;; Types.

(ert-deftest test-company-tern-function-type ()
  (let ((candidate "function")
        (type "fn(test: fn(elt: ?, i: number) -> bool, context?: ?) -> bool")
        company-tooltip-align-annotations)
    (put-text-property 0 1 'type type candidate)
    (should (s-equals? (company-tern-get-type candidate)
                       "(test, context?)"))))

(ert-deftest test-company-tern-function-align-type ()
  (let ((candidate "function")
        (type "fn(test: fn(elt: ?, i: number) -> bool, context?: ?) -> bool")
        (company-tooltip-align-annotations t))
    (put-text-property 0 1 'type type candidate)
    (should (s-equals? (company-tern-get-type candidate)
                       "fn(test, context?)"))))

(ert-deftest test-company-tern-variable-type ()
  (let ((candidate "variable")
        company-tooltip-align-annotations)
    (put-text-property 0 1 'type "?" candidate)
    (should (s-equals? " -> ?" (company-tern-get-type candidate)))))

(ert-deftest test-company-tern-variable-align-type ()
  (let ((candidate "variable")
        (company-tooltip-align-annotations t))
    (put-text-property 0 1 'type "?" candidate)
    (should (s-equals? "?" (company-tern-get-type candidate)))))

(ert-deftest test-company-tern-keyword-type ()
  (let ((candidate "keyword"))
    (put-text-property 0 1 'isKeyword t candidate)
    (should-not (company-tern-get-type candidate))))

;;; Functions.

(ert-deftest test-company-tern-function-p ()
  (let ((candidate "function"))
    (put-text-property 0 1 'type "fn()" candidate)
    (should (company-tern-function-p candidate))))

(ert-deftest test-company-tern-not-a-function-p ()
  (let ((candidate "unknown"))
    (put-text-property 0 1 'type "?" candidate)
    (should-not (company-tern-function-p candidate))))

;;; Properties.

(ert-deftest test-company-tern-property-p ()
  (let ((candidate "property"))
    (put-text-property 0 1 'isProperty t candidate)
    (put-text-property 0 1 'depth 0 candidate)
    (should (company-tern-property-p candidate))))

(ert-deftest test-company-tern-not-a-property-p ()
  (let ((candidate "other"))
    (put-text-property 0 1 'isProperty json-false candidate)
    (should-not (company-tern-property-p candidate))))

;;; Keywords.

(ert-deftest test-company-tern-keyword-p ()
  (let ((candidate "keyword"))
    (put-text-property 0 1 'isKeyword t candidate)
    (should (company-tern-keyword-p candidate))))

(ert-deftest test-company-tern-not-a-keyword ()
  (let ((candidate "variable"))
    (should-not (company-tern-keyword-p candidate))))

;;; Process candidates.

(ert-deftest test-company-tern-format-candidates ()
  (let ((candidate (car (company-tern-format-candidates
                         '((completions . [((isKeyword . t) (depth . 0) (name . "var"))])
                           (isProperty . nil)
                           (end . 1)
                           (start . 0))))))
    (should (s-equals? candidate "var"))
    (should (get-text-property 0 'isKeyword candidate))
    (should-not (get-text-property 0 'isProperty candidate))))

;;; Sort by depth.

(ert-deftest test-company-tern-sort-by-depth ()
  (let* ((data '(("aaa" . 2) ("bbb" . 0) ("ccc" . 1)))
         (candidates (--map (progn
                              (put-text-property 0 1 'depth (cdr it) (car it))
                              (car it))
                            data)))
    (should (equal (company-tern-sort-by-depth candidates)
                   '("bbb" "ccc" "aaa")))))

(provide 'company-tern-test)

;;; company-tern-test.el ends here
