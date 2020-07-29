;;; -*- lexical-binding: t; -*-

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
    (expect (schema-validation-map (lambda (value) (* 2 value)) (schema-validation-failure))
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

(describe "validator output ap"
  (it "returns right if both succeed"
    (expect (schema-validation-ap (schema-validation-success 1) (schema-validation-success 2))
            :to-equal
            (schema-validation-success 2)))
  (it "fails if left fails"
    (expect (schema-validation-ap (schema-validation-failure) (schema-validation-success 2))
            :to-equal
            (schema-validation-failure)))
  (it "fails if right fails"
    (expect (schema-validation-ap (schema-validation-success 1) (schema-validation-failure))
            :to-equal
            (schema-validation-failure))))

(describe "validator output traverse*"
  (it "succeeds on empty input"
    (expect (schema-validation-traverse* (schema _) nil)
            :to-equal (schema-validation-success t)))

  (it "fails if any fail"
    (expect (schema-validation-traverse* (schema stringp) (list (schema-validation-failure)))
            :to-equal (schema-validation-failure))
    (expect (schema-validation-traverse* (schema stringp) (list (schema-validation-failure)
                                                                (schema-validation-failure)))
            :to-equal (schema-validation-failure))
    (expect (schema-validation-traverse* (schema stringp) (list (schema-validation-success 1)
                                                                (schema-validation-failure)))
            :to-equal (schema-validation-failure)))

  (it "succeeds if all succeed"
    (expect (schema-validation-traverse* (schema _) (list (schema-validation-success 1)))
            :to-equal (schema-validation-success t))
    (expect (schema-validation-traverse*
             (schema _)
             (list (schema-validation-success 1)
                   (schema-validation-success 2)
                   (schema-validation-success 3)))
            :to-equal (schema-validation-success t))))

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

  (describe "wildcard (_)"
    (it "passes anything"
      (expect-pass (schema _) t)
      (expect-pass (schema _) nil)
      (expect-pass (schema _) '(1 2 3))))

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
    (it "passes tautologies"
      (expect-pass (schema (or 0 0)) 0))
    (it "passes when matching left"
      (expect-pass (schema (or 0 1)) 0))
    (it "passes when matching right"
      (expect-pass (schema (or 0 1)) 1))
    (it "passes when final term matches"
      (expect-pass (schema (or 0 1 2)) 2))
    (it "fails when matching neither"
      (expect-fail (schema (or 0 1)) 2)))

  (describe "refinements (and)"
    (it "rejects nullary and"
      (expect-compile-fail '(and)))
    (it "passes trivial singleton"
      (expect-pass (schema (and 0)) 0))
    (it "fails contradictions"
      (expect-fail (schema (and 0 1)) 0))
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
      (expect-pass (schema (not (not 0))) 0)))

  (describe "positional sequence validators"
    (it "empty seq"
      (expect-pass (schema []) nil)
      (expect-pass (schema []) [])
      (expect-pass (schema []) ""))
    (it "singleton seq"
      (expect-pass (schema [symbolp]) '(a))
      (expect-pass (schema [symbolp]) [a])
      (expect-pass (schema [characterp]) "a")
      (expect-fail (schema [symbolp]) '("a"))
      (expect-fail (schema [symbolp]) ["a"]))
    (it "different lengths"
      (expect-fail (schema []) "a")
      (expect-fail (schema [symbolp symbolp]) ["a"]))
    (it "mixed types"
      (expect-pass (schema [symbolp symbolp]) '(a a))
      (expect-fail (schema [symbolp]) '(a a))
      (expect-pass (schema [symbolp _ symbolp]) '(a 1 a))
      (expect-pass (schema [stringp integerp]) '("hello" 1))
      (expect-fail (schema [integerp stringp]) '("hello" 1)))

    (it "complicated nesting"
      (expect-pass (schema [(or stringp cl-plusp) symbolp]) '(1 a))
      (expect-pass (schema [(and numberp (lambda (it) (< it 5)))]) '(1)))))

(describe "defining schemas"
  (before-all
    (eval '(progn
             (schema-define test-schema-1
               numberp
               "Example schema.")

             (schema-define test-schema-2
               test-schema-1)

             (schema-define test-schema-foo-bar-baz
               (or "foo" "bar" "baz"))

             (schema-define test-complex
               (or test-schema-foo-bar-baz
                   (and keywordp (not :baz))
                   'baz
                   (and numberp (lambda (it) (< it 5))))))))

  (it "can be called directly"
    (expect (test-schema-1 1) :to-equal (schema-validation-success 1))
    (expect (test-schema-1 "foo") :to-equal (schema-validation-failure)))

  (it "can refer to other schemas"
    (expect (test-schema-2 1) :to-equal (schema-validation-success 1))
    (expect (test-schema-2 "foo") :to-equal (schema-validation-failure)))

  (it "handles complex schemas"
    (expect (test-complex "foo") :to-equal (schema-validation-success "foo"))
    (expect (test-complex :foo) :to-equal (schema-validation-success :foo))
    (expect (test-complex 4) :to-equal (schema-validation-success 4))
    (expect (test-complex "invalid") :to-equal (schema-validation-failure))
    (expect (test-complex 5) :to-equal (schema-validation-failure))
    (expect (test-complex 'baz) :to-equal (schema-validation-success 'baz))
    (expect (test-complex :baz) :to-equal (schema-validation-failure))
    (expect (test-complex nil) :to-equal (schema-validation-failure))))

(describe "sequence types"
  (it "cons"
    (expect-pass (schema (cons _ _))
                 '("foo" . "bar"))
    (expect-pass (schema (cons numberp listp))
                 '(1 . ("bar"))))
  (it "seq"
    (expect-pass (schema (seq _)) '())
    (expect-pass (schema (seq _)) [])

    (expect-pass (schema (seq _)) '(a :b "c"))
    (expect-pass (schema (seq _)) [a :b "c"])
    (expect-pass (schema (seq 'a)) '(a a a a))
    (expect-pass (schema (seq 'a)) [a a a a])
    (expect-pass (schema (seq characterp)) "hello")

    (expect-fail (schema (seq _)) 1)

    (expect-fail (schema (seq 'a)) '(a a b))
    (expect-fail (schema (seq 'a)) [a a b])

    (expect-fail (schema (seq stringp)) '(a))
    (expect-fail (schema (seq stringp)) '("a" b))
    (expect-fail (schema (seq stringp)) [a]))

  (it "list"
    (expect-pass (schema (list _)) '())
    (expect-pass (schema (list _)) '(a :b "c"))
    (expect-pass (schema (list 'a)) '(a))

    (expect-fail (schema (list _)) [])
    (expect-fail (schema (list 'a)) [a])
    (expect-fail (schema (list characterp)) "hello"))

  (it "vector"
    (expect-pass (schema (vector _)) [])
    (expect-pass (schema (vector 'a)) [a])

    (expect-fail (schema (vector _)) '())
    (expect-fail (schema (vector _)) '(a :b "c"))
    (expect-fail (schema (vector 'a)) '(a)))

  (it "alist"
    (expect-pass (schema (alist _ _)) nil)
    (expect-pass (schema (alist 'a _)) '((a) (a . b)))
    (expect-pass (schema (alist _ _)) '((a) (:b) ("c")))

    (expect-fail (schema (alist a _)) [(a) (a)])
    (expect-fail (schema (alist symbolp _)) '((a) (:b) ("c"))))

  (it "plist"
    (expect-pass (schema (plist _ _)) nil)
    (expect-fail (schema (plist _ _)) '(a))

    (expect-pass (schema (plist _ _)) '(a b))
    (expect-pass (schema (plist 'a _)) '(a :b a :c))
    (expect-pass (schema (plist symbolp keywordp)) '(a :b a :c))
    (expect-fail (schema (plist symbolp symbolp)) [a b c d]))

  (it "hash"
    (expect-pass (schema (hash _ _)) #s(hash-table))
    (expect-fail (schema (hash _ _)) nil)

    (expect-pass (schema (hash _ _)) #s(hash-table data (a 1)))
    (expect-pass (schema (hash 'a _)) #s(hash-table data (a 1)))
    (expect-pass (schema (hash symbolp keywordp)) #s(hash-table data (a :a b :b)))
    (expect-fail (schema (hash symbolp stringp)) #s(hash-table data (a :a b :b)))
    (expect-fail (schema (hash symbolp keywordp)) '(a :a b :b)))

  (it "map"
    (expect-pass (schema (map _ _)) #s(hash-table))
    (expect-pass (schema (map _ _)) '(:a a :b b))
    (expect-pass (schema (map _ _)) '((a . "a") (b . "b")))

    (expect-pass (schema (map 'a 1)) #s(hash-table data (a 1)))
    (expect-pass (schema (map symbolp keywordp)) '(a :a b :b))
    (expect-fail (schema (map symbolp stringp)) '(a :a b :b))
    (expect-fail (schema (map symbolp keywordp)) [a :a b :b]))

  (it "optional"
    (expect-pass (schema (optional 1)) nil)
    (expect-pass (schema (optional 1)) 1)
    (expect-fail (schema (optional 2)) 1)))

(describe "numbers"
  (it "gt/gte"
    (expect-fail (schema (num :min 1)) 0)
    (expect-fail (schema (num :min 1)) 0.5)
    (expect-pass (schema (num :min 1)) 1)
    (expect-pass (schema (num :min 1)) 1.5)
    (expect-pass (schema (num :min 1)) 2)
    (expect-fail (schema (num :gte 1)) 0)
    (expect-fail (schema (num :gte 1)) 0.5)
    (expect-pass (schema (num :gte 1)) 1)
    (expect-pass (schema (num :gte 1)) 1.5)
    (expect-pass (schema (num :gte 1)) 2)
    (expect-fail (schema (num :gt 1)) 0)
    (expect-fail (schema (num :gt 1)) 1)
    (expect-pass (schema (num :gt 1)) 1.5)
    (expect-pass (schema (num :gt 1)) 2)
    (expect-pass (schema (num :gt 1)) 2.5))

  (it "lt/lte"
    (expect-pass (schema (num :max 1)) 0)
    (expect-pass (schema (num :max 1)) 0.5)
    (expect-pass (schema (num :max 1)) 1)
    (expect-fail (schema (num :max 1)) 1.5)
    (expect-fail (schema (num :max 1)) 2)

    (expect-pass (schema (num :lte 1)) 0)
    (expect-pass (schema (num :lte 1)) 0.5)
    (expect-pass (schema (num :lte 1)) 1)
    (expect-fail (schema (num :lte 1)) 1.5)
    (expect-fail (schema (num :lte 1)) 2)

    (expect-pass (schema (num :lt 1)) 0)
    (expect-pass (schema (num :lt 1)) 0.5)
    (expect-fail (schema (num :lt 1)) 1)
    (expect-fail (schema (num :lt 1)) 1.5)))
