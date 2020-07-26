(require 'buttercup)
(require 'schema)

(defun expect-pass (s a)
  (expect (schema-validate s a) :to-equal a))

(defun expect-fail (s a)
  (expect (schema-validate s a) :to-throw 'schema-validation-error))

(defun expect-compile-fail (s)
  (expect (schema-compile s) :to-throw 'schema-compilation-error))

(describe "functor on validator output"
  (it "preserves failed value"
    (expect (schema-validation-map (lambda (value) (* 2)) (schema-validation-failure))
            :to-equal (schema-validation-failure)))

  (it "succeeds if successful value"
    (expect (schema-validation-map (lambda (value) (* 2 value)) (schema-validation-success 2))
            :to-equal (schema-validation-success 4)))

  (it "law: identity"
    (let ((input (schema-validation-success t)))
      (expect (schema-validation-map #'identity input) :to-equal input)))

  (it "law: composition"
    (let ((input (schema-validation-success 3)))
      (expect (schema-validation-map (lambda (value) (* 3 (* 2 value))) input)
              :to-equal
              (schema-validation-map (lambda (value) (* 3 value))
                               (schema-validation-map (lambda (value) (* 2 value)) input))))))

(describe "monadic join on validator output"
  (it "succeeds if both are successful"
    (expect (schema-validation-join (schema-validation-success (schema-validation-success t)))
            :to-equal (schema-validation-success t)))

  (it "fails if inner error"
    (expect (schema-validation-join (schema-validation-success (schema-validation-failure)))
            :to-equal (schema-validation-failure)))

  (it "fails if outer failure"
    (expect (schema-validation-join (schema-validation-failure))
            :to-equal (schema-validation-failure)))

  (it "law: associativity"
    (let ((r (schema-validation-success (schema-validation-success (schema-validation-success t)))))
      (expect (schema-validation-join (schema-validation-map #'schema-validation-join r))
              :to-equal
              (schema-validation-join (schema-validation-join r)))))
  (it "law: applicativity"
    (let ((r (schema-validation-success (schema-validation-success t))))
      (expect (schema-validation-join (schema-validation-map #'schema-validation-success r))
              :to-equal
              (schema-validation-join (schema-validation-success r))))))

(describe "validating"

  (describe "number literals"
    (it "passes for eq literal"
      (expect-pass (schema 0) 0)
      (expect-pass (schema 1) 1)
      (expect-pass (schema 2) 2))
    (it "fails for different literal"
      (expect-fail (schema 1) 2)))

  (describe "string literals"
    (it "passes for eq literal"
      (expect-pass (schema "") "")
      (expect-pass (schema "foo") "foo"))
    (it "fails for different literal"
      (expect-fail (schema "foo") "bar")))

  (describe "keyword literals"
    (it "passes for eq literal"
      (expect-pass (schema :foo) :foo))
    (it "fails for different literal"
      (expect-fail (schema :foo) :bar)))

  (describe "symbol literals"
    (it "passes for eq literal"
      (expect-pass (schema 'foo) 'foo))
    (it "fails for different literal"
      (expect-fail (schema 'foo) 'bar))
    (it "fails for unquoted literal (interpreted as a function)"
      (expect-fail (schema not) 'not)))

  (describe "inline predicate functions"
    (it "numbers pass numberp"
      (expect-pass (schema numberp) 1))
    (it "strings fail numberp"
      (expect-fail (schema numberp) "foo")))

  (describe "alternatives (or)"
    (it "rejects nullary or"
      (expect-compile-fail '(or)))
    (it "passes trivial singleton"
      (expect-pass (schema (or 0)) 0))
    (it "passes when matching left"
      (expect-pass (schema (or 0 1)) 0))
    (it "passes when matching right"
      (expect-pass (schema (or 0 1)) 1))
    (it "passes when matching both"
      (expect-pass (schema (or 0 0)) 0))
    (it "passes when final term matches"
      (expect-pass (schema (or 0 1 2)) 2))
    (it "fails when matching neither"
      (expect-fail (schema (or 0 1)) 2)))

  (describe "refinements (and)"
    (it "rejects nullary and"
      (expect-compile-fail '(and)))
    (it "passes trivial singleton"
      (expect-pass (schema (and 0)) 0))
    (it "passes when matching both"
      (expect-pass (schema (and 0 0)) 0))
    (it "fails when matching only left"
      (expect-fail (schema (and 0 1)) 0))
    (it "fails when matching only right"
      (expect-fail (schema (and 0 1)) 1)))

  (describe "negation (not)"
    (it "accepts only one argument"
      (expect-compile-fail '(not))
      (expect-compile-fail '(not 1 1)))
    (it "single negation"
      (expect-pass (schema (not 0)) 1)
      (expect-fail (schema (not 0)) 0))
    (it "double-negation"
      (expect-fail (schema (not (not 0))) 1)
      (expect-pass (schema (not (not 0))) 0))))

(describe "defining schemas"
  (schema-define test-schema-1
    numberp
    "Example schema.")

  (it "can be called directly"
    (expect (test-schema-1 1) :to-equal (schema-validation-success 1))
    (expect (test-schema-1 "foo") :to-equal (schema-validation-failure)))

  (schema-define test-schema-2
    test-schema-1)

  (it "can refer to other schemas"
    (expect (test-schema-2 1) :to-equal (schema-validation-success 1))
    (expect (test-schema-2 "foo") :to-equal (schema-validation-failure))))
