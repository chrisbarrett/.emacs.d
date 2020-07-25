(require 'buttercup)
(require 'schema)

(describe "Validating a number"
  (it "number literals validate against themselves"
    (expect (schema-validate (schema 0) 0) :to-be 0)
    (expect (schema-validate (schema 1) 1) :to-be 1)
    (expect (schema-validate (schema 2) 2) :to-be 2)))
