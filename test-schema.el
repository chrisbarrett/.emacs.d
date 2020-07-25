(require 'buttercup)
(require 'schema)

(defun expect-pass (s a)
  (expect (schema-validate s a) :to-equal a))

(defun expect-fail (s a)
  (expect (schema-validate s a) :to-throw 'validation-error))


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

  (describe "alternatives"
    (it "passes when matching left"
      (expect-pass (schema (or 0 1)) 0))

    (it "passes when matching right"
      (expect-pass (schema (or 0 1)) 1))

    (it "passes when matching both"
      (expect-pass (schema (or 0 0)) 0))

    (it "fails when matching neither"
      (expect-fail (schema (or 0 1)) 2))))
