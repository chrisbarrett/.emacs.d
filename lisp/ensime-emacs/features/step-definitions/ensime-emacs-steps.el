;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'ensime-company) ;; the autoload cookie is not available in unit tests

(When "^I open temp \\(java\\|scala\\) file \"\\(.+\\)\"$"
      (lambda (suffix arg)
        (find-file (make-temp-file arg nil (format ".%s" suffix)))))

(When "^I insert import \"\\(.+\\)\"$"
     (lambda (import)
       (ensime-insert-import import)))

(When "^I go to the end of the line$"
      (lambda () (end-of-line)))

(Then "^I should have an empty completion prefix$"
      (lambda ()
        (let ((expected (ensime-completion-prefix-at-point)))
          (assert (equal "" expected) nil
                  (format "Expected prefix %s, but was %s" "" expected)))))

(Then "^I should have completion prefix \"\\(.+\\)\"$"
      (lambda (prefix)
        (let ((expected (ensime-completion-prefix-at-point)))
          (assert (equal prefix expected) nil
                  (format "Expected prefix %s, but was %s" prefix expected)))))

(Then "^I should see unique prefixes:"
      (lambda (prefixes)
        (-each (cdr prefixes)
          (-lambda ((prefix))
            (When (format "I place the cursor after %S" prefix))
            (Then (format "I should have completion prefix %S" prefix))))))

(Then "^I should see prefixes at the end of lines:"
      (lambda (prefixes-lines)
        (-each (cdr prefixes-lines)
          (-lambda ((prefix line))
            (When (format "I go to line %S" line))
            (And (format "I go to the end of the line"))
            (Then (format "I should have completion prefix %S" prefix)))))) 

(Given "^I got a search result for \"\\(.+\\)\" in file \"\\(.+\\)\" in line \"\\([0-9]+\\)\"$"
     (lambda (class file linenumber)
       (let ((line (string-to-int linenumber)))
         (setq ensime-helm-test-search-result (ensime-helm--format-search-elemen `(:type "type" :name ,class :local-name ,class :decl-as "class" :pos (:type line :file ,file :line ,line)))))))

(Then "^the formatted result should match$"
     (lambda (formatted-result)
          (assert (equal ensime-helm-test-search-result formatted-result) nil
                  (format "Expected formated result \"%s\", but was \"%s\"" formatted-result ensime-helm-test-search-result))))

(Then "^the line should match \"\\(.+\\)\"$"
     (lambda (line)
       (let ((actual-line (s-trim (thing-at-point 'line))))
         (assert (equal line actual-line) nil
                 (format "Expected \"%s\", but was \"%s\"" line actual-line)))))

(Given "^I got a type hierarchy type for \"\\(.+\\)\" in file \"\\(.+\\)\" in line \"\\([0-9]+\\)\"$"
     (lambda (class file linenumber)
       (let ((line (string-to-int linenumber)))
         (setq ensime-helm-test-hierarchy-type-result `(:fqn ,class :source-position (:type line :file ,file :line ,line))))))

(Then "^I format the hierarchy type in the current buffer$"
     (lambda ()
       (ensime-write-hierarchy-entries-to-buffer `(,ensime-helm-test-hierarchy-type-result))))
