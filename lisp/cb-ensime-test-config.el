;;; cb-ensime-test-config.el --- Config for ensime test support.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'ensime)

(defconst cb-ensime-test-config-test-file-template
  "import org.scalatest.{ BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpec }

class %TESTCLASS% extends WordSpec with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  override def beforeAll() = {
  }

  after {
  }

  \"\" should {
    \"\" in pending
  }

}
"
  "The default value to insert into new scala test buffers.
See `ensime-goto-test-config-defaults' for possible template values.")

(defconst cb-ensime-test-config-defaults
  (list :test-class-names-fn #'ensime-goto-test--test-class-names
        :test-class-suffixes '("Test" "Tests"
                               "IntTest" "IntTests" "IntegrationTest" "IntegrationTests"
                               "Spec" "Specs" "Specification" "Specifications"
                               "Prop" "Props" "Property" "Properties"
                               "Check" "Checks")
        :impl-class-name-fn #'ensime-goto-test--impl-class-name
        :impl-to-test-dir-fn #'ensime-goto-test--impl-to-test-dir
        :is-test-dir-fn #'ensime-goto-test--is-test-dir
        :test-template-fn (lambda () cb-ensime-test-config-test-file-template)))

(provide 'cb-ensime-test-config)

;;; cb-ensime-test-config.el ends here
