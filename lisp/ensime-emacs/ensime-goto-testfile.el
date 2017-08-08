;;; ensime-goto-testfile.el  -- Navigate to test classes

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'cl-lib)
(require 'scala-mode-syntax)

(defun ensime-goto-test (&optional other-window-p)
  "Locate the test file that corresponds to the class around the point,
and visit that file. If the test file doesn't exist, it is created and
filled with a stub test class.
With an argument, open the test file in another window."
  (interactive "P")
  (block nil
    (when (ensime-is-test-file (buffer-file-name-with-indirect))
      (message "This isn't an implementation file")
      (return))
    (let* ((impl-class
            (or (ensime-top-level-class-closest-to-point)
                (return (message "Could not find top-level class"))))
           (test-class-info
            (or (ensime-get-test-class-info impl-class)
                (return (message "Could not determine test class for %s"
                                 impl-class)))))
      (cond
       ((equal impl-class "<none>.<none>")
        (message "Hit ensime race condition, please try again"))
       ((listp test-class-info) (ensime-goto-source-location test-class-info))
       ((stringp test-class-info)
        (ensime-create-test-file test-class-info impl-class other-window-p))))))

(defun ensime-goto-impl (&optional other-window-p)
  "If the point is inside a test class, go to the corresponding
implementation class. With an argument, open the test file in another window."
  (interactive "P")
  (block nil
    (unless (ensime-is-test-file (buffer-file-name-with-indirect))
      (message "This isn't a test file")
      (return))
    (let ((test-class (ensime-top-level-class-closest-to-point)))
      (cond
       ((equal test-class "<none>.<none>")
        (message "Hit ensime race condition, please try again"))
       ((null test-class)
        (message "Could not find top-level class"))
       (t
        (ensime-goto-source-location (ensime-get-impl-class-info test-class)))))))

(defun ensime-get-goto-test-config (key)
  (let* ((module-name (plist-get (ensime-config (ensime-connection)) :name))
         (case-fold-search nil)
         (module-params
          (cdr
           (cl-find module-name ensime-goto-test-configs
                 :test (lambda (m p) (string-match-p (car p) m))))))
    (if (plist-member module-params key)
        (plist-get module-params key)
      (plist-get ensime-goto-test-config-defaults key))))

(defun ensime-source-base-dir-for-file (file-name)
  "Return the source base directory for the current buffer, as defined in the
ensime configuration."
  (let* ((all-sources
          (mapcar #'expand-file-name (ensime-source-roots-from-config)))
         (dir
          (find-if
           (lambda (dir) (ensime-path-includes-dir-p file-name dir))
           all-sources)))
      (when dir (file-name-as-directory (expand-file-name dir)))))

(defun ensime-is-test-file (file-name)
  "Return true if the given file name is part of the project's test sources"
  (funcall (ensime-get-goto-test-config :is-test-dir-fn)
           (ensime-source-base-dir-for-file file-name)))

(defun ensime-top-level-class-closest-to-point ()
  "Return the name of first class, trait or object enclosing the point,
or (if the point isn't inside a class definition) the class that follows
the point. Return nil if no class can be found."
  ;; TODO use an RPC call instead of this cheesy search
  (cl-labels
      ((inside-string? () (nth 3 (syntax-ppss)))
       (pos-of-top-level-class (&optional last-try)
         (save-excursion
           (save-restriction
             (widen)
             (while (inside-string?)
               (goto-char (1- (point))))
             (let ((top-level-sexp (point)))
               ;; Try to go up a sexp until we get an error
               (condition-case nil
                   (while t
                     (setq top-level-sexp (point))
                     (backward-up-list))
                 (error nil))
               (goto-char top-level-sexp)

               (re-search-backward "}\\|\\<object\\>\\|\\<class\\>\\|\\<trait\\>" nil t)
               (let ((class-re
                      (concat "\\<\\(object\\|class\\|trait\\)[ \t\n]+\\("
                              scala-syntax:id-re
                              "\\)")))
                 (if (re-search-forward class-re nil t)
                     (match-beginning 2)
                   (unless last-try
                     (pos-of-top-level-class t)))))))))
    (let ((pos (pos-of-top-level-class)))
      (when pos
        (save-excursion
          (goto-char pos)
          (replace-regexp-in-string
           "\\$$" ""
           (plist-get (ensime-rpc-get-type-at-point) :full-name)))))))

(defun ensime-get-test-class-info (impl-class)
  "Return information for the test class that correspondss to IMPL-CLASS.
If the return value is
- a list: it contains the position of an existing class within the project.
- a string: it names a new test class that must be created
- nil: a test class could not be determined."
  (let* ((candidates (funcall (ensime-get-goto-test-config :test-class-names-fn)
                              impl-class))
         (positions (mapcar
                     (lambda (c)
                       (plist-get
                        (or
                         (ensime-rpc-get-type-by-name c)
                         (ensime-rpc-get-type-by-name (concat c "$")))
                        :pos))
                     candidates))
         (found-position
          (find-if (lambda (pos)
                     (and pos
                          (ensime-pos-file pos)
                          (ensime-is-test-file (ensime-pos-file pos))))
                   positions)))
    (or found-position
        (first candidates))))

(defun ensime-get-impl-class-info (test-class)
  "Return the location the implementation class that correspondss to TEST-CLASS.
Return a position list, or nil if the implementation class couldn't be
determined."
  (let* ((candidate (funcall (ensime-get-goto-test-config :impl-class-name-fn)
                             test-class))
         (position (and candidate
                        (plist-get
                         (or
                          (ensime-rpc-get-type-by-name candidate)
                          (ensime-rpc-get-type-by-name (concat candidate "$")))
                         :pos))))
    (when (and position
               (ensime-pos-file position)
               (not (ensime-is-test-file (ensime-pos-file position))))
      position)))

(defun ensime-create-test-file (test-class-name impl-class-name &optional other-window-p)
  "Create a file for the class TEST-CLASS-NAME and switch to it. Populate
the file with stub code. if the file already exists, simply visit it."
  (block nil
    (let ((test-file-name (ensime-get-test-file-name test-class-name))
          (impl-coding buffer-file-coding-system))
      (unless test-file-name
        (message "Could not determine test file name for %s" test-class-name)
        (return))

      (make-directory (file-name-directory test-file-name) t)

      (if other-window-p
          (find-file-other-window test-file-name)
        (find-file test-file-name))

      (unless (file-exists-p test-file-name)
        (ensime-generate-test-stub test-class-name impl-class-name)
        (set-buffer-file-coding-system impl-coding)))))

(defun ensime-get-test-file-name (test-class-name)
  "Return the name of the file that should contain the test class
TEST-CLASS-NAME. The current buffer must be the file that contains the
implementation class."
  (let* ((impl-base-dir (ensime-source-base-dir-for-file (buffer-file-name-with-indirect)))
         (impl-extension (file-name-extension (buffer-file-name-with-indirect) t))
         (test-relative-path
          (concat
           (replace-regexp-in-string "\\." "/" test-class-name)
           impl-extension)))
    (when impl-base-dir
      (let ((test-base-dir
             (funcall (ensime-get-goto-test-config :impl-to-test-dir-fn)
                      impl-base-dir)))
        (when test-base-dir
          (expand-file-name test-relative-path test-base-dir))))))

(defun ensime-generate-test-stub (test-class-name impl-class-name)
  "Insert stub test code in the current buffer, for the class TEST-CLASS-NAME"
  (let (test-package test-class impl-package impl-class template
        (case-fold-search nil))
    (string-match "^\\(\\(.*\\)\\.\\)?\\([^.]+\\)$" test-class-name)
    (setq test-package (match-string 2 test-class-name))
    (setq test-class (match-string 3 test-class-name))

    (string-match "^\\(\\(.*\\)\\.\\)?\\([^.]+\\)$" impl-class-name)
    (setq impl-package (match-string 2 impl-class-name))
    (setq impl-class (match-string 3 impl-class-name))

    (setq template (funcall (ensime-get-goto-test-config :test-template-fn)))

    (setq template (replace-regexp-in-string "%TESTPACKAGE%" test-package template t))
    (setq template (replace-regexp-in-string "%IMPLPACKAGE%" impl-package template t))
    (setq template (replace-regexp-in-string "%TESTCLASS%" test-class template t))
    (setq template (replace-regexp-in-string "%IMPLCLASS%" impl-class template t))

    (insert template)))

(defun ensime-goto-test--test-class-names (impl-class)
  (let ((suffixes (ensime-get-goto-test-config :test-class-suffixes)))
    (mapcar
       (lambda (s)
         (replace-regexp-in-string "^\\(.*\\)$"
                                   (concat "\\1" s)
                                   impl-class t))
       suffixes)))

(defun ensime-goto-test--impl-class-name (test-class)
  (let ((suffixes (ensime-get-goto-test-config :test-class-suffixes))
        (case-fold-search nil))
    (dolist (s suffixes)
      (when (string-match-p (concat s "$") test-class)
        (return
         (replace-regexp-in-string (concat s "$") "" test-class t))))))

(defun ensime-goto-test--impl-to-test-dir (impl-dir)
  (let ((conf (ensime-config (ensime-connection)))
        (is-test-dir-fn (ensime-get-goto-test-config :is-test-dir-fn)))
    (dolist (module (plist-get conf :subprojects))
      (let ((module-sources
             (mapcar (lambda (s)
                       (file-name-as-directory (expand-file-name s)))
                     (plist-get module :source-roots))))
        (when (cl-find impl-dir module-sources :test #'equal)
          (return (find-if is-test-dir-fn module-sources)))))))

(defun ensime-goto-test--is-test-dir (dir)
  (let ((case-fold-search nil))
    (or
     (string-match-p "src/test/scala/$" dir)
     (string-match-p "src/it/scala/$" dir)
     (string-match-p "src/fun/scala/$" dir)
     (string-match-p "/tests?/$" dir))))

(defun ensime-goto-test--test-template-scalatest-2 ()
  ""
  "package %TESTPACKAGE%

import org.scalatest.FunSpec
import org.scalatest.Matchers

class %TESTCLASS% extends FunSpec with Matchers {
  describe (\"%IMPLPACKAGE%.%IMPLCLASS%\") {
    it(\"should have a test!\") {
      assert(1 === 0)
    }
  }
}
")

(defun ensime-goto-test--test-template-scalatest-1 ()
  ""
  "package %TESTPACKAGE%

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class %TESTCLASS% extends FunSpec with ShouldMatchers {
  describe (\"%IMPLPACKAGE%.%IMPLCLASS%\") {
    it(\"should have a test!\") {
      assert(1 === 0)
    }
  }
}
")

(defun ensime-goto-test--test-template-scalatest-wordspec ()
  "ENSIME template for a ScalaCheck WordSpec style test."
  "package %TESTPACKAGE%

import org.scalatest._

class %TESTCLASS% extends WordSpec with Matchers {
  \"%IMPLCLASS%\" should {
    \"have a test!\" in {
      fail(\"no test\")
    }
  }
}
")

(defun ensime-goto-test--test-template-scalatest-flatspec ()
  "ENSIME template for a ScalaCheck FlatSpec style test."
  "package %TESTPACKAGE%

import org.scalatest._

class %TESTCLASS% extends FlatSpec with Matchers {
  \"%IMPLCLASS%\" should \"have a test!\" in {
    fail(\"no test\")
  }
}
")


(defun ensime-goto-test--test-template-scalacheck ()
  ""
  "package %TESTPACKAGE%

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import %IMPLPACKAGE%.%IMPLCLASS%

object %TESTCLASS% extends Properties(\"%IMPLPACKAGE%.%IMPLCLASS%\") {

  property(\"test\") = forAll { x: %IMPLCLASS% =>
    l == 0
  }

}")

(defun ensime-goto-test--test-template-specs2 ()
  ""
  "package %TESTPACKAGE%

import org.specs2.mutable._
import %IMPLPACKAGE%.%IMPLCLASS%

class %TESTCLASS% extends Specification {

  \"A %IMPLPACKAGE%.%IMPLCLASS%\" should {
    \"have a test\" in {
      1 must beEqualTo(0)
    }
  }

}")

(provide 'ensime-goto-testfile)

;; Local Variables:
;; End:
