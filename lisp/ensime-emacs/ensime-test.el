;;; ensime-test.el --- Regression tests for ENSIME
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(eval-and-compile (require 'ensime))

(defvar ensime-testing-buffer "*ensime-tests*"
  "Contains the output of all the tests. Also tracks all the testing-specific
   buffer-local variables.")

(defvar ensime-test-queue '()
  "The queue of tests yet to be run.")
(make-variable-buffer-local 'ensime-test-queue)

(defvar ensime-async-handler-stack '()
  "Asynchronous event handlers waiting for signals. See 'ensime-test-sig'.")
(make-variable-buffer-local 'ensime-async-handler-stack)

(defvar ensime-shared-test-state '()
  "A state dump for anyone who wants to use it. Useful for async tests.")
(make-variable-buffer-local 'ensime-shared-test-state)

(defvar ensime-test-dev-home
  (expand-file-name "./")
  "The local development root.")

(defvar ensime-test-env-classpath '()
  "Extra jars to include on testing classpath")

(defvar ensime--test-had-failures nil)

(defvar ensime--test-exit-on-finish noninteractive)

(defvar ensime--test-pending-rpcs nil)

(put 'ensime-test-assert-failed
     'error-conditions '(error ensime-test-assert-failed))
(put 'ensime-test-assert-failed 'error-message "Assertion Failed")

(put 'ensime-test-interrupted
     'error-conditions '(error ensime-test-interrupted))
(put 'ensime-test-interrupted 'error-message "Test Interrupted")


(defun ensime-test-concat-lines (&rest lines)
  (mapconcat #'identity lines "\n"))


(defun ensime-create-file (file-name contents)
  "Create file named file-name. Write contents to the file. Return file's name."
  (make-directory (file-name-directory file-name) t)
  (with-temp-file file-name
    (insert contents))
  file-name)

(defun ensime-test-find-overlays-specifying (prop)
  "Return a list of overlays that specify property PROP in buffer."
  (let ((overlays (overlays-in (point-min) (point-max)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defvar ensime--test-scala-version
  (or (getenv "SCALA_VERSION") "2.11.8"))

(defun ensime-create-tmp-project
    (src-files &optional extra-config subproject-name extra-subproject-dirs)
  "Create a temporary project directory. Populate with config, source files.
 Return a plist describing the project. Note: Delete such projects with
 ensime-cleanup-tmp-project."
  (let* ((root-dir (make-temp-file "ensime_test_proj_" t))
         (cache-dir (expand-file-name "cache" root-dir))
         (src-dir (expand-file-name "src/main/scala" root-dir))
         (unit-test-dir (expand-file-name "src/test/scala" root-dir))
         (int-test-dir (expand-file-name "src/it/scala" root-dir))
         (target-dir (expand-file-name
		      (concat "target/scala-"
                      (ensime--scala-binary-version ensime--test-scala-version)
                      "/classes")
                      root-dir))
         (test-target-dir (expand-file-name "test-target" root-dir))
         (example-dotensime (ensime-config-load "test/example/.ensime"))
         (scala-compiler-jars (plist-get example-dotensime :scala-compiler-jars))
         (ensime-server-jars (plist-get example-dotensime :ensime-server-jars))
         (ensime-server-version (plist-get example-dotensime :ensime-server-version))
         (scala-jar (-find (lambda (e) (s-contains? "scala-library" e)) scala-compiler-jars))
	 (sp-name (if subproject-name
		      subproject-name
		    (downcase (file-name-nondirectory root-dir))))
         (config (let ((env (getenv "ENSIME_JVM_TEST_FLAGS"))
                       (default-flags '("-Xmx4g" "-Xss2m")))
                   (append
                    extra-config
                    `(:root-dir ,root-dir
                                :cache-dir ,cache-dir
                                :name "test"
                                :scala-version ,ensime--test-scala-version
                                :ensime-server-jars ,ensime-server-jars
                                :ensime-server-version ,ensime-server-version
                                :scala-compiler-jars ,scala-compiler-jars
                                :java-home ,(getenv "JAVA_HOME")
                                :java-flags , (if env
                                                  (cons env default-flags)
                                                default-flags)
                                :subprojects ((:name ,sp-name
                                                     :module-name ,sp-name
                                                     :source-roots (,src-dir ,unit-test-dir ,int-test-dir)
                                                     :depends-on-modules nil
                                                     :compile-deps (,scala-jar)
                                                     :targets (,target-dir)
                                                     :test-targets (,test-target-dir)))))))
                 (conf-file (ensime-create-file
                             (expand-file-name ".ensime" root-dir)
                             (format "%S" config))))

    (mkdir src-dir t)
    (mkdir unit-test-dir t)
    (mkdir cache-dir t)
    (mkdir target-dir t)
    (mkdir test-target-dir t)
    (mapcar (lambda (d) (-> (expand-file-name d root-dir) (mkdir t))) extra-subproject-dirs)

    (when ensime--test-cached-project
      (when ensime--debug-messages
        (message "Copying %s to %s"
                 (plist-get ensime--test-cached-project :cache-dir)
                 cache-dir))
      (copy-directory (plist-get ensime--test-cached-project :cache-dir)
		      cache-dir nil t t)
      (when (file-exists-p (expand-file-name "port" cache-dir))
	(delete-file (expand-file-name "port" cache-dir))))

    (let* ((src-file-names
	    (mapcar
	     (lambda (f)
	       (let* ((name (plist-get f :name))
		      (contents (plist-get f :contents))
		      (relative (plist-get f :relative-to))
		      (relative-dir (if relative
					(expand-file-name relative root-dir)
				      src-dir))
		      (file-name (expand-file-name name relative-dir)))
		 (ensime-create-file file-name contents)))
	     src-files)))
      (list
       :src-files src-file-names
       :root-dir root-dir
       :cache-dir cache-dir
       :conf-file conf-file
       :src-dir src-dir
       :targets `(,target-dir)
       :config config))))

(defun ensime-create-tmp-project-compiled (src-files)
  (let ((proj (ensime-create-tmp-project src-files)))
    (find-file (car (plist-get proj :src-files)))
    (ensime-sbt-do-compile)
    (sleep-for 25)
    proj))

(defvar ensime-tmp-project-hello-world
  `((:name
     "hello_world.scala"
     :contents ,(ensime-test-concat-lines
                 "package com.helloworld"
                 "class HelloWorld{"
                 "}"
                 "object HelloWorld {"
                 "def main(args: Array[String]) = {"
                 "Console.println(\"Hello, world!\")"
                 "}"
                 "def foo(a:Int, b:Int):Int = {"
                 "a + b"
                 "}"
                 "}"))))

(defun ensime-test-compile-java-proj (proj arguments)
  "Compile java sources of given temporary test project."
  (let* ((root (plist-get proj :root-dir))
         (src-files (plist-get proj :src-files))
         (target (car (plist-get proj :targets)))
         (args (append
                arguments
                (list "-d" target)
                src-files)))
    (assert (executable-find "javac"))
    (assert (= 0 (apply 'call-process "javac" nil "*javac*" nil args)))))

(defun ensime-cleanup-tmp-project (proj &optional no-del)
  "Destroy a temporary project directory, kill all buffers visiting
   source files in the project."
  ;; TODO Should delete all buffers that weren't there at the test start
  (let ((src-files (plist-get proj :src-files))
        (root-dir (plist-get proj :root-dir)))
    (dolist (f src-files)
      (cond ((file-exists-p f)
             (let ((buf (find-buffer-visiting f)))
               (when buf
                 (switch-to-buffer buf)
                 (set-buffer-modified-p nil)
                 (kill-buffer nil))))
            ((get-buffer f)
             (progn
               (switch-to-buffer (get-buffer f))
               (kill-buffer nil)))
            (t)))

    (when (and (not no-del)
               root-dir
               ;; a bit of paranoia..
               (or (integerp (string-match "^/tmp/" root-dir))
                   (integerp (string-match "/Temp/" root-dir))))
      ;; ..before we wipe away the project dir
      (when ensime--debug-messages
        (message "Deleting %s" root-dir))
      (with-current-buffer ensime-testing-buffer
        (delete-directory root-dir t)))))

(defun ensime-kill-all-ensime-servers ()
  "Kill all ensime server buffers."
  (let ((case-fold-search nil))
    (dolist (b (buffer-list))
      (when (string-match (concat "^\\*" ensime-default-buffer-prefix ".*") (buffer-name b))
        (kill-buffer b)))))

(defmacro ensime-test-var-put (var val)
  "Helper for writing to shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (setq ensime-shared-test-state
           (plist-put ensime-shared-test-state ,var ,val))))

(defmacro ensime-test-var-get (var)
  "Helper for reading from shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (plist-get ensime-shared-test-state ,var)))

(defun ensime--return-event-handler (value id)
  (setq ensime--test-pending-rpcs
        (remove id ensime--test-pending-rpcs)))

(defun ensime--send-event-handler (value id)
  (push id ensime--test-pending-rpcs))

(defun ensime-test-sig (event value)
  "Driver for asynchonous tests. This function is invoked from ensime core,
   signaling events to events handlers installed by asynchronous tests."
  (when (buffer-live-p (get-buffer ensime-testing-buffer))
    (when ensime--debug-messages
      (message "pending rpcs: %s" ensime--test-pending-rpcs))
    (when (> (length ensime--test-pending-rpcs) 1)
      (message "WARNING more than one pending message"))
    (when ensime--debug-messages
      (message "Received test event: %s" event))
    (with-current-buffer ensime-testing-buffer
      (when (not (null ensime-async-handler-stack))
        (let* ((ensime-prefer-noninteractive t)
               (handler (car ensime-async-handler-stack))
               (guard-func (plist-get handler :guard-func))
               (handler-events (plist-get handler :events)))
          (cond
           (ensime-stack-eval-tags
            (when ensime--debug-messages
              (message "Got %s/%s while in a synchronous call, ignoring"
                       event value)))

           ((and (equal (list event) handler-events)
                 (or (null guard-func) (funcall guard-func value)))
            (let ((handler-func (plist-get handler :func))
                  (is-last (plist-get handler :is-last)))
              (when ensime--debug-messages
                (message "Handling test event: %s/%s" event handler))
              (pop ensime-async-handler-stack)
              (save-excursion
                (condition-case signal
                    (funcall handler-func value)
                  (ensime-test-interrupted
                   (message
                    "Error executing test: %s" signal)
                   (when ensime--test-exit-on-finish
                     (kill-emacs 1))
                   (setq is-last t))))
              (when is-last
                (setq ensime-async-handler-stack nil)
                (pop ensime-test-queue)
                (ensime-run-next-test))))

           ((and (> (length handler-events) 1)
		 (member event handler-events))
            (when ensime--debug-messages
              (message "Received %s/%s expecting %s with guard func %s. Waiting for the rest"
                       event value handler-events guard-func))
            (setq handler-events (cl-remove event handler-events :count 1))
            (setq handler (plist-put handler :events handler-events))
            (setcar ensime-async-handler-stack handler))

           (t
            (when ensime--debug-messages
              (message "Got %s/%s, expecting %s with guard %s. Ignoring event."
                       event value handler-events guard-func)))))))))


(defun ensime-run-suite (suite)
  "Run a sequence of tests."
  (switch-to-buffer ensime-testing-buffer)
  (erase-buffer)
  (setq ensime-test-queue suite)
  (let ((ensime--test-cached-project nil))
    (ensime-run-next-test)))

(defmacro ensime-test-suite (&rest tests)
  "Define a sequence of tests to execute.
   Tests may be synchronous or asynchronous."
  `(list ,@tests))


(defmacro ensime-test (title &rest body)
  "Define a synchronous test."
  `(list :title ,title :async nil
         :func (lambda ()
                 (ensime-test-run-with-handlers
                  ,title
                  ,@body))))

(defmacro ensime-test-run-with-handlers (context &rest body)
  "Evaluate body in the context of an error handler. Handle errors by
   writing to the testing output buffer."
  `(save-excursion
     (condition-case signal
         (progn ,@body)
       (ensime-test-assert-failed
	(setq ensime--test-had-failures t)
        (message "Assertion failed at '%s': %s" ,context signal)
        (signal
         'ensime-test-interrupted
         (format "Test interrupted: %s." signal))))))


(defmacro* ensime-async-test (title trigger &rest handlers)
  "Define an asynchronous test. Tests have the following structure:
   (ensime-async-test
    title
    trigger-expression
    [handler]*
   )
   Where:
   title is a string that describes the test.
   trigger-expression is some expression that either constitutes the entire
     test, or (as is more common) invokes some process that will yield an
     asynchronous event.
   handler can be of one of two forms:
    1) (event-types body...) where:
     event-types is a list keywords that identifies event classes ; or a single keyword
     body... is a list of arbitrary expressions

    2) (event-type value-name guard-expr body...) where
     event-type  is a single keyword that identifies the expected event class
     value-name  is the symbol to which to bind the event payload
     guard-expression is an expression evaluated with value-name bound to
          the payload.
     body... is a list of arbitrary expressions evaluated with value-name bound to the
       payload of the event.

   When the test is executed, trigger-expression is evaluated. The test then
   waits for an asynchronous test event. When an event is signalled, the next
   handler in line is considered.
   - If the type of the event doesn't belong to the event-types of the handler head,
     it is ignored
   - Otherwise the event is recorded as being seen
   - Once all the events in the handler's event-types have been seen, the corresponding
     handler body is executed. The order in which the events are seen doesn't matter.

   Handlers must be executed in order, they cannot be skipped. The test will
   wait in an unfinished state until an event is signalled that matches the
   next handler in line.
   "
  (let* ((last-handler (car (last handlers)))
         (handler-structs
          (mapcar
           (lambda (h)
             (if (listp (car h))
                 (list
                  :events (car h)
                  :func `(lambda (,(gensym)) (ensime-test-run-with-handlers ,title ,@(cdr h)))
                  :is-last (eq h last-handler))
               (let ((value-sym (nth 1 h)))
                 (list
                  :events (list (car h))
                  :guard-func `(lambda (,value-sym) ,(nth 2 h))
                  :func `(lambda (,value-sym) (ensime-test-run-with-handlers ,title ,@(nthcdr 3 h)))
                  :is-last (eq h last-handler)))))
           handlers))
         (trigger-func
          `(lambda ()
             (ensime-test-run-with-handlers
              ,title
              ,trigger))))
    `(list :title ,title :async t
           :trigger ,trigger-func
           :handlers ',handler-structs)))

(defun ensime-run-next-test ()
  "Run the next test from the test queue."
  (ensime-kill-all-ensime-servers)
  (with-current-buffer ensime-testing-buffer
    (if ensime-test-queue
        (let ((ensime-prefer-noninteractive t)
              (test (car ensime-test-queue)))
          (setq ensime-shared-test-state '())
          (setq ensime-async-handler-stack '())
          (message "\n")
          (message "Starting test: '%s'" (plist-get test :title))
          (if (plist-get test :async)
              ;; Asynchronous test
              (let ((handlers (reverse (plist-get test :handlers))))
                (dolist (h handlers)
                  (push h ensime-async-handler-stack))
                (funcall (plist-get test :trigger)))
            ;; Synchronous test
            (progn
              (pop ensime-test-queue)
              (save-excursion
                (condition-case signal
                    (funcall (plist-get test :func))
                  (ensime-test-interrupted
                   (message "Error executing test, moving to next."))))
              (ensime-run-next-test))))
      (progn
        (goto-char (point-max))
        (let* ((status (if ensime--test-had-failures 1 0))
               (msg (format "Finished suite with status %s." status)))
          (message msg)
          (when ensime--test-cached-project
            (ensime-cleanup-tmp-project ensime--test-cached-project))
          (when ensime--test-exit-on-finish
            (kill-emacs status)))))))

(defmacro ensime-assert (pred)
  `(let ((val ,pred))
     (if (not val)
         (with-current-buffer ensime-testing-buffer
           (signal 'ensime-test-assert-failed
                   (format "Expected truth of %s." ',pred))))))


(defmacro ensime-assert-equal (a b)
  `(let ((val-a ,a)
         (val-b ,b))
     (if (equal val-a val-b) t
       (with-current-buffer ensime-testing-buffer
         (signal 'ensime-test-assert-failed
                 (format "Expected %s to equal %S, but was %S." ',a val-b val-a))))))

(defun ensime-assert-file-contains-string (f str)
  (with-temp-buffer
    (insert-file-contents-literally f)
    (goto-char (point-min))
    (ensime-assert (search-forward str nil t))))

(defun ensime-stop-tests ()
  "Forcibly stop all tests in progress."
  (interactive)
  (with-current-buffer ensime-testing-buffer
    (setq ensime-async-handler-stack nil)
    (setq ensime-test-queue nil))
  (switch-to-buffer ensime-testing-buffer))

(defun ensime-test-eat-label (mark)
  (goto-char (point-min))
  (when (search-forward-regexp (concat "/\\*" mark "\\*/") nil t)
    (kill-backward-chars (+ 4 (length mark)))))

(defun ensime-test-after-label (mark)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (concat "/\\*" mark "\\*/") nil t)
      (point))))

(defun ensime-test-before-label (mark)
  (- (ensime-test-after-label mark) (+ 5 (length mark))))


(defmacro* ensime-test-with-proj ((proj-name src-files-name) &rest body)
  "Evaluate body in a context where the current test project is bound
 to proj-name, the src-files of the project are bound to src-files-name,
 and the active buffer is visiting the first file in src-files."
  `(let* ((,proj-name (ensime-test-var-get :proj))
          (,src-files-name (plist-get ,proj-name :src-files))
          (existing (remove-if-not #'file-exists-p ,src-files-name)))
     (when existing
       (find-file (car existing)))
     ,@body))

(defun ensime-test-init-proj (proj &optional no-init)
  "Store the project in a test var. Load the source files, switch to
 the first source file, and optionally init ensime."
  (let ((src-files (plist-get proj :src-files)))
    (ensime-test-var-put :proj proj)
    (ensime-test-var-put :pending-rpcs nil)
    (if src-files
	(find-file (car src-files))
      (dired (plist-get proj :root-dir)))
    (unless no-init (ensime))))

(defun ensime-test-cleanup (proj &optional no-del)
  "Delete temporary project files. Kill ensime buffers."
  (when ensime--test-pending-rpcs
    (message "WARNING no response to messages: %s . Waiting some more."
	     ensime--test-pending-rpcs)
    (sleep-for 1))
  (if ensime--test-pending-rpcs
      (progn
        (message "ERROR no response to messages: %s"
                 ensime--test-pending-rpcs)
        (setq ensime--test-had-failures t))
    (when ensime--debug-messages
      (message "OK no unreplied messages")))
  (ensime-kill-all-ensime-servers)
					; In Windows, we can't delete cache files until the server process has exited
  (sleep-for 3)
  (ensime-cleanup-tmp-project proj no-del))

;;;;;;;;;;;;;;;;;;
;; ENSIME Tests ;;
;;;;;;;;;;;;;;;;;;

(defun ensime--test-completions ()
  "Helper for completion testing."
  (plist-get (ensime-get-completions 30 nil) :candidates))

(defvar ensime--test-cached-project
  "When set, indicates a project that can be reused to setup tests"
  nil)

;; WORKAROUND https://github.com/capitaomorte/yasnippet/issues/697
(defun yas-mock-insert (string)
  (require 'ert-x)
  (dotimes (i (length string))
    (let ((last-command-event (aref string i)))
      (ert-simulate-command '(self-insert-command 1)))))

(defvar ensime-slow-suite

  (ensime-test-suite

   (ensime-async-test
    "Cache index."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "test.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack.a"
                                 "class A {"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)
      (setq ensime--test-cached-project proj)
      (ensime-test-cleanup proj t))))

   (ensime-async-test
    "Test edit definition."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "test.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack.a"
                                 "case class A(value: String)"
                                 ""
                                 "object B {"
                                 "  val testA = A(\"moep\")"
                                 "  /*1*/testA.value"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :indexer-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (goto-char (ensime-test-after-label "1"))
      (ensime-edit-definition-of-thing-at-point)
      (ensime-assert-equal (thing-at-point 'symbol) "testA")
      (goto-char (ensime-test-after-label "1"))
      (ensime-edit-definition-of-type-of-thing-at-point)
      (ensime-assert-equal (thing-at-point 'symbol) "A")
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test completing members."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"

                                 "class HelloWorld{"
                                 "  def foo(a:Int, b:Int):Int = {"
                                 "    HelloWorld./*1*/"
                                 "  }"
                                 "  def bar(a:Int, b:Int):Int = {"
                                 "    val v = HelloWorld./*2*/"
                                 "    foo(1,v)"
                                 "  }"
                                 "}"

                                 "object HelloWorld {"
                                 "  def blarg = 5"
                                 "  def add(a:Int, b:Int) = {"
                                 "    System.out.pri/*3*/"
                                 "    a + b"
                                 "  }"
                                 "  def test() {"
                                 "    val dude = \"hello\""
                                 "    System.out.println(dude./*4*/)"
                                 "  }"
                                 "  def test2() = {"
                                 "    val dude = \"hello\""
                                 "    dude.substring(2,2).hea/*5*/"
                                 "  }"
                                 "}")))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; object method completion
      (ensime-test-eat-label "1")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "add" candidates)))

      ;; Try completion when a method begins without target
      ;; on next line.
      (ensime-test-eat-label "2")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "blarg" candidates)))

      ;; Instance completion with prefix
      (ensime-test-eat-label "3")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "println" candidates)))

      ;; Complete member of argument
      (ensime-test-eat-label "4")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "substring" candidates)))

      ;; Chaining of calls
      (ensime-test-eat-label "5")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "headOption" candidates)))

      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test completing symbols."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "import java.io.File"

                                 "class HelloWorld{"

                                 "  def main {"
                                 "    val f = new Fi/*1*/"
                                 "  }"

                                 "  def blarg:Int = 5"

                                 "  def add(a:Int):Int = {"
                                 "    val x = a + bl/*2*/"
				 "    x + blar/*3*/"
                                 "  }"

                                 "}")))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)

      ;; constructor completion
      (ensime-test-eat-label "1")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "File" candidates)))

      ;; local method name completion.
      (ensime-test-eat-label "2")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "blarg" candidates)))

      ;; exercize emacs CAPF function
      ;; (which involves loading the ensime-autocomplete file and enabling CAPF)
      (require 'ensime-auto-complete)
      (add-hook 'completion-at-point-functions
                'ensime-completion-at-point-function nil t)
      (ensime-test-eat-label "3")
      (completion-at-point)
      (ensime-assert-equal
       "blarg" (buffer-substring-no-properties (- (point) 5) (point)))

      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test expanding parameter lists."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"

                                 "object HelloWorld {"
                                 "  private var _field:Int = 1"
                                 "  def field = _field"
                                 "  def field_=(i:Int) { _field = i}"
                                 "  def add(a:Int, b:Int) = {"
                                 "    a + b"
                                 "  }"
                                 "  def str(i:Int) = {"
                                 "    i.toString"
                                 "  }"
                                 "  def doBlock(block:(Int => String)) = {"
                                 "    block(1)"
                                 "  }"
                                 "  def doByName(block: => String) = {"
                                 "    block"
                                 "  }"
                                 "  def test(): Unit = {"
                                 "    val c = ad/*1*/"
                                 "    val d = c /*2*/"
                                 "    val e = d./*3*/"
                                 "    val f = doBlo/*4*/"
                                 "    val g = doBlo/*5*/"
                                 "    val h = doByNa/*6*/"
                                 "      this.fie/*7*/"
                                 "    \"kjsdf\".hashCo/*8*/"
                                 "    \"kjsdf\".getByt/*8_1*/"
                                 "    5.toLo/*9*/"
                                 "  }"
                                 "}")))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand simple, two argument list.
      (ensime-test-eat-label "1")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "add" candidates))
	(yas-mock-insert "d")
	(ensime--yasnippet-complete-action (car (member "add" candidates)))
	(yas-mock-insert "2") (yas-next-field) (yas-mock-insert "3") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "d(2, 3)"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand operator.
      (ensime-test-eat-label "2")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "+" candidates))
	(yas-mock-insert "+")
	(ensime--yasnippet-complete-action (car (member "+" candidates)))
	(yas-mock-insert "5") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "+ 5"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand operator after typing '.'
      (ensime-test-eat-label "3")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "+" candidates))
	(yas-mock-insert "+")
	(ensime--yasnippet-complete-action (car (member "+" candidates)))
	(yas-mock-insert "8") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties (- pt 1) (point)) " + 8"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand a function taking a named function as argument.
      (ensime-test-eat-label "4")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "doBlock" candidates))
	(yas-mock-insert "ck")
	(ensime--yasnippet-complete-action (car (member "doBlock" candidates)) ?\()
	(yas-mock-insert "str") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "ck(str)"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand a function taking a function block as argument.
      (ensime-test-eat-label "5")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "doBlock" candidates))
	(yas-mock-insert "ck")
	(ensime--yasnippet-complete-action (car (member "doBlock" candidates)) ?\{)
	(yas-mock-insert "i") (yas-next-field) (yas-mock-insert "str") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "ck { i => str }"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand a function taking a by name block.
      (ensime-test-eat-label "6")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "doByName" candidates))
	(yas-mock-insert "me")
	(ensime--yasnippet-complete-action (car (member "doByName" candidates)) ?\{)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "me { ")
	(yas-mock-insert "\"bla\""))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand a field assignment
      (ensime-test-eat-label "7")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "field_=" candidates))
	(yas-mock-insert "ld_=")
	(ensime--yasnippet-complete-action (car (member "field_=" candidates)) ?\{)
	(yas-mock-insert "2") (yas-next-field)
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "ld = 2"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand an empty argument list for java method.
      (ensime-test-eat-label "8")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "hashCode" candidates))
	(yas-mock-insert "de")
	(ensime--yasnippet-complete-action (car (member "hashCode" candidates)))
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "de()"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand an empty argument list for java getter method.
      (ensime-test-eat-label "8_1")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "getBytes" candidates))
	(yas-mock-insert "es")
	(ensime--yasnippet-complete-action (car (member "getBytes" candidates)))
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "es"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      ;; Expand an no argument list for nullary scala method
      (ensime-test-eat-label "9")
      (let* ((candidates (ensime--test-completions))
	     (pt (point)))
        (ensime-assert (member "toLong" candidates))
	(yas-mock-insert "ng")
	(ensime--yasnippet-complete-action (car (member "toLong" candidates)))
	(ensime-assert-equal
	 (buffer-substring-no-properties pt (point)) "ng"))
      (ensime-save-and-typecheck-current-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-test-cleanup proj))))


   (ensime-async-test
    "Test completing imports."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "import scala.collection.imm/*1*/"
                                 ;;"import Vec/*3*/"
                                 "import scala.collection.immutable.{ List, Vec/*4*/}"
                                 "class HelloWorld{"
                                 "import sc/*2*/"
                                 "}")))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)

      (find-file (car src-files))

      ;; complete package member
      (ensime-test-eat-label "1")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "immutable" candidates)))
      (insert "utable.List")

      ;; complete package member by class name in name list
      (ensime-test-eat-label "4")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "Vector" candidates)))

      ;; complete scala package in class body
      (ensime-test-eat-label "2")
      (let* ((candidates (ensime--test-completions)))
        (ensime-assert (member "scala" candidates)))

      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test organize imports diff refactoring: remove unused import."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "import scala.collection.immutable.Vector"
                                 "class HelloWorld{"
                                 "}"
                                 ""))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-refactor-diff-organize-imports)))
    (:refactor-diff-done diff t
                            (ensime-test-with-proj
                             (proj src-files)
                             (find-file (car src-files))
                             (let ((src (buffer-substring-no-properties
                                         (point-min) (point-max))))
                               (ensime-assert-equal src (ensime-test-concat-lines
                                                         "package com.helloworld"
                                                         "class HelloWorld{"
                                                         "}"
                                                         "")))
                             (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test rename diff refactoring over multiple files."
    (let* ((proj (ensime-create-tmp-project-compiled
                  `((:name
                     "hello_world.scala"
                     :relative-to "src/main/scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "class /*1*/HelloWorld{"
                                 "}"
                                 ""))
                    (:name
                     "another.scala"
                     :relative-to "src/main/scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "object Another {"
                                 "def main(args:Array[String]) {"
                                 "val a = new HelloWorld()"
                                 "}"
                                 "}"
                                 ""))
                    (:name
                     "build.sbt"
                     :relative-to ""
                     :contents ,(ensime-test-concat-lines
                                  (concat "scalaVersion := \"" ensime--test-scala-version "\"")
                                  (concat "scalaBinaryVersion := \"" (ensime--scala-binary-version ensime--test-scala-version) "\"")))))))
      (ensime-test-init-proj proj))
    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-reload-open-files)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-assert (null (ensime-all-notes))))
     (goto-char (ensime-test-after-label "1"))
     (forward-char)
     (ensime-refactor-diff-rename "DudeFace"))

    (:refactor-diff-done diff t
                            (ensime-test-with-proj
                             (proj src-files)
                             (find-file (car src-files))
                             (let ((src (buffer-substring-no-properties
                                         (point-min) (point-max))))
                               (ensime-assert-equal src (ensime-test-concat-lines
                                                         "package com.helloworld"
                                                         "class /*1*/DudeFace{"
                                                         "}"
                                                         "")))
                             (find-file (car (cdr src-files)))
                             (let ((src (buffer-substring-no-properties
                                         (point-min) (point-max))))
                               (ensime-assert-equal src (ensime-test-concat-lines
                                                         "package com.helloworld"
                                                         "object Another {"
                                                         "def main(args:Array[String]) {"
                                                         "val a = new DudeFace()"
                                                         "}"
                                                         "}"
                                                         "")))
                             (ensime-test-cleanup proj))))
   (ensime-async-test
    "Test find-references."
    (let* ((proj (ensime-create-tmp-project-compiled
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class /*1*/A(value:String){"
                                 "}"))
                    (:name
                     "pack/b.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class B(value:String) extends A(value){"
                                 "}"))
                    (:name
                     "build.sbt"
                     :relative-to ""
                     :contents ,(ensime-test-concat-lines
                                  (concat "scalaVersion := \"" ensime--test-scala-version "\"")
                                  (concat "scalaBinaryVersion := \"" (ensime--scala-binary-version ensime--test-scala-version) "\"")))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-reload-open-files)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-test-eat-label "1")
      (save-buffer)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-show-uses-of-symbol-at-point)))

    ((:references-buffer-shown)
     (switch-to-buffer ensime-uses-buffer-name)
     (goto-char (point-min))
     (ensime-assert (search-forward "class B(value:String) extends A" nil t))
     (funcall (key-binding (kbd "q")))
     (ensime-test-cleanup proj)))

   (ensime-async-test
    "Test file with package name (this broke when -sourcepath param was used)."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack.a"
                                 "class A(value:String){"
                                 "}"))
                    (:name
                     "pack/b.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack.a"
                                 "class B(value:String) extends A(value){"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-reload-open-files)))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (let* ((notes (ensime-all-notes)))
        (ensime-assert-equal (length notes) 0))
      (ensime-test-cleanup proj))))

   ;; (ensime-async-test
   ;;  "Test deleting file and reloading."
   ;;  (let* ((proj (ensime-create-tmp-project
   ;;                `((:name
   ;;                   "pack/a.scala"
   ;;                   :contents ,(ensime-test-concat-lines
   ;;                               "package pack"
   ;;                               "class A(value:String){"
   ;;                               "}"))
   ;;                  (:name
   ;;                   "pack/b.scala"
   ;;                   :contents ,(ensime-test-concat-lines
   ;;                               "package pack"
   ;;                               "class B(value:String) extends A(value){"
   ;;                               "}"))))))
   ;;    (ensime-test-init-proj proj))


   ;;  ((:connected))
   ;;  ((:compiler-ready :full-typecheck-finished)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (ensime-reload-open-files)))

   ;;  ((:full-typecheck-finished)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (let* ((notes (ensime-all-notes)))
   ;;      (ensime-assert-equal (length notes) 0))
   ;;    (kill-buffer nil)
   ;;    (delete-file (car src-files))
   ;;    (find-file (cadr src-files))))

   ;;  ((:full-typecheck-finished)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (ensime-reload-open-files)))

   ;;  ((:full-typecheck-finished)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (let* ((notes (ensime-all-notes)))
   ;;      (ensime-assert (> (length notes) 0)))
   ;;    (ensime-test-cleanup proj))))

   (ensime-async-test
    "Verify re-typecheck on save-buffer."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (let* ((notes (ensime-all-notes)))
        (ensime-assert-equal (length notes) 0)
        (find-file (car src-files))
        (goto-char (point-min))
        (insert "lksdjfldkjf ")

        ;; save-buffer should trigger a recheck...
        (save-buffer))))

    ((:full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (let ((notes (ensime-all-notes)))
        (ensime-assert (> (length notes) 0))
        (ensime-test-cleanup proj)))))

   (ensime-async-test
    "Test get symbol info at point."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class A {"
                                 "  def foo(/*2*/a:Int, b:Int):Int = {"
                                 "    a/*1*/ + b"
                                 "  }"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (goto-char (ensime-test-before-label "1"))
      (let* ((info (ensime-rpc-symbol-at-point))
             (pos (ensime-symbol-decl-pos info)))
        (ensime-assert-equal
         (ensime-pos-offset pos)
         (ensime-externalize-offset (ensime-test-after-label "2"))))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test interactive search."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world)))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)
      ;; Prevent a previous search from affecting this test
      (setq ensime-search-text "")
      (ensime-search)
      (insert (propertize "scala.collection.immutable.Vector" 'face 'italic))))

    ((:search-buffer-populated)
     (ensime-test-with-proj
      (proj src-files)

      (with-current-buffer ensime-search-target-buffer-name
        ;; uncomment to see the results (e.g. if they change due to server improvements)
        ;;(message "%s" (buffer-string))
        (goto-char 1)
        (ensime-assert (search-forward-regexp "scala.collection.immutable.Vector[[:space:]]+" nil t))
        (goto-char 1)
        ;; I don't necessarilly agree with these results, indeed they will change when
        ;; we refactor the search backend.
        (ensime-assert (search-forward "scala.collection.immutable.VectorIterator" nil t)))

      (ensime-search-quit)
      (ensime-test-cleanup proj))))


   ;; (ensime-async-test
   ;;  "Test misc operations on unsaved source file."
   ;;  (let* ((proj (ensime-create-tmp-project '())))
   ;;    (ensime-test-init-proj proj))

   ;;  ((:connected))
   ;;  ((:compiler-ready :full-typecheck-finished :indexer-ready)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (let ((path (expand-file-name "src/main/scala/test/Test.scala" (plist-get proj :root-dir))))
   ;;  (ensime-test-var-put :path path)
   ;;      (make-directory (file-name-directory path) t)
   ;;      (find-file path)
   ;;      (insert (ensime-test-concat-lines
   ;;                               "package test"
   ;;                               "class A(value:String){"
   ;;                               "def hello(){"
   ;;                               "  println/*1*/(1)"
   ;;                               "  /*2*/"
   ;;                               "}"
   ;;                               "}"))
   ;;  (ensime-mode)
   ;;      (ensime-assert-equal (ensime-owning-connection-for-source-file path)
   ;;                           (ensime-owning-connection-for-rootdir
   ;;                            (plist-get proj :root-dir)))

   ;;      (ensime-assert (= (length (ensime-all-notes)) 0))

   ;;  ;; Verify nothing we did caused the file to be written.
   ;;  (ensime-assert (not (file-exists-p path)))
   ;;  )))

   ;;  ((:full-typecheck-finished :region-sem-highlighted)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (let ((path (ensime-test-var-get :path)))
   ;;  (find-file path)
   ;;      (ensime-assert (= (length (ensime-all-notes)) 0))
   ;;  (goto-char (ensime-test-before-label "2"))
   ;;      (insert "prin")

   ;;      (ensime-typecheck-current-buffer)
   ;;      (ensime-sem-high-refresh-buffer)

   ;;  ;; Verify nothing we did caused the file to be written.
   ;;  (ensime-assert (not (file-exists-p path)))
   ;;      )))

   ;;  ((:full-typecheck-finished :region-sem-highlighted)
   ;;   (ensime-test-with-proj
   ;;    (proj src-files)
   ;;    (let ((path (ensime-test-var-get :path)))
   ;;  (find-file path)

   ;;  ;; Auto typecheck should catch unrecognized symbol
   ;;  (ensime-assert (> (length (ensime-all-notes)) 0))

   ;;  ;; Auto semantic highlighting should have kicked in
   ;;  (goto-char (ensime-test-before-label "1"))
   ;;  (ensime-assert (memq 'functionCall (ensime-sem-high-sym-types-at-point)))

   ;;  ;; Completion should work
   ;;  (goto-char (ensime-test-before-label "2"))
   ;;  (let* ((candidates (ensime--test-completions)))
   ;;        (ensime-assert (member "println" candidates)))

   ;;  ;; Verify nothing we did caused the file to be written.
   ;;  (ensime-assert (not (file-exists-p path)))

   ;;  ;; Extra steps to kill unsaved file without complaint.
   ;;  (set-buffer-modified-p nil)
   ;;  (kill-buffer nil)

   ;;  (ensime-test-cleanup proj))))
   ;;  )

   (ensime-async-test
    "Test add import."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class A(value:String){"
                                 "def hello(){"
                                 "  println(new /*1*/ListBuffer())"
                                 "}"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)
      (goto-char 1)
      (ensime-assert (null (search-forward "import scala.collection.mutable.ListBuffer" nil t)))

      (goto-char (ensime-test-after-label "1"))
      (ensime-import-type-at-point t)

      (goto-char 1)
      (ensime-assert (search-forward "import scala.collection.mutable.ListBuffer" nil t))

      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test expand-selection."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class A(value:String){"
                                 "def hello(){"
                                 "  println(/*1*/\"hello\")"
                                 "}"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (ensime-test-eat-label "1")
      (ensime-save-buffer-no-hooks)

      ;; Expand once to include entire string
      (let* ((pt (point))
             (range (ensime-rpc-expand-selection
                     buffer-file-name
                     pt pt))
             (start1 (plist-get range :start))
             (end1 (plist-get range :end)))
        (ensime-assert (= start1 pt))
        (ensime-assert (> end1 pt))

        ;; Expand again to include entire println call
        (let* ((range (ensime-rpc-expand-selection
                       buffer-file-name
                       start1 end1))
               (start2 (plist-get range :start))
               (end2 (plist-get range :end)))
          (ensime-assert (< start2 start1))
          (ensime-assert (> end2 end1))))

      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test semantic highlighting."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "import java.io.File"
                                 "class A(value:String) extends /*9*/Object{"
                                 "  case class Dude(name:Integer)"
                                 "  val myTick/*7*/ = 0"
                                 "  var myTock/*8*/ = 0"
                                 "  def hello(){"
                                 "    var tick/*2*/ = 1"
                                 "    val tock/*6*/ = 2"
                                 "    /*5*/println(new /*1*/File(\".\"))"
                                 "    /*3*/tick = /*4*/tick + 1"
                                 "    val d = /*10*/Dude(1)"
                                 "    d match{"
                                 "      case /*11*/Dude(i) => {}"
                                 "      case o:/*12*/Object => {}"
                                 "      case _ => {}"
                                 "    }"
                                 "  }"
                                 "}"
                                 "class B {}"
                                 "class C{"
                                 "  implicit def stringToB(s: String) = new B"
                                 "  val x: B = \"xxx\"/*13*/"
                                 "}"
                                 "class D {"
                                 "  @deprecated(\"BadClass\", \"1.0\")"
                                 "  class BadClass {}"
                                 "  class Subclass extends BadClass/*14*/"
                                 "  @deprecated(\"foo\", \"1.0\")"
                                 "  def foo(x: Int) = x + 1"
                                 "  val x = foo/*15*/(41)"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :region-sem-highlighted)
     (ensime-test-with-proj
      (proj src-files)
      (let ((check-sym-is (lambda (sym-type)
                            (ensime-assert
                             (memq
                              sym-type
                              (ensime-sem-high-sym-types-at-point))))))
        (goto-char (ensime-test-after-label "1"))
        (funcall check-sym-is 'class)

        (goto-char (ensime-test-before-label "2"))
        (funcall check-sym-is 'var)

        (goto-char (ensime-test-after-label "3"))
        (funcall check-sym-is 'var)

        (goto-char (ensime-test-after-label "4"))
        (funcall check-sym-is 'var)

        (goto-char (ensime-test-after-label "5"))
        (funcall check-sym-is 'functionCall)

        (goto-char (ensime-test-before-label "6"))
        (funcall check-sym-is 'val)

        (goto-char (ensime-test-before-label "7"))
        (funcall check-sym-is 'valField)

        (goto-char (ensime-test-before-label "8"))
        (funcall check-sym-is 'varField)

        (goto-char (ensime-test-after-label "9"))
        (funcall check-sym-is 'class)

        (goto-char (ensime-test-after-label "10"))
        (funcall check-sym-is 'object)

        (goto-char (ensime-test-after-label "11"))
        (funcall check-sym-is 'object)

        (goto-char (ensime-test-after-label "12"))
        (funcall check-sym-is 'class)

        (goto-char (ensime-test-before-label "13"))
        (funcall check-sym-is 'implicitConversion)

        (goto-char (ensime-test-before-label "14"))
        (funcall check-sym-is 'deprecated)

        (goto-char (ensime-test-before-label "15"))
        (funcall check-sym-is 'deprecated))

      (ensime-test-cleanup proj))))


   (ensime-async-test
    "Test ensime imenu index function."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "import java.io.File"
                                 "class Test(val accessor: File)"
                                 "case class CaseTest(x: String, y: Int)"
                                 "object Test {"
                                 "  type TestType[A] = List[A]"
                                 "  class Nested(val accessor: String)"
                                 "  case class NestedCase(x: String, y:Int)"
                                 "  implicit def stringToNested(s: String) = new Nested(s)"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :indexer-ready :full-typecheck-finished)

     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (goto-char (point-min))
      (let ((expected '(("class:Test" . 40)
                        ("class:CaseTest" . 76)
                        ("object:Test" . 111)
                        ("type:Test.TestType" . 125)
                        ("class:Test.Nested" . 155)
                        ("class:Test.NestedCase" . 197)
                        ("def:Test.stringToNested" . 241)))
	    (imenu-index (ensime-imenu-index-function)))
	(ensime-assert (not (null imenu-index)))
	(ensime-assert-equal imenu-index expected))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test implicit notes."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "pack/a.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package pack"
                                 "class B {}"
                                 "class C{"
                                 " implicit def stringToB(s: String)(implicit x: Int) = new B"
                                 " implicit val zz: Int = 1"
                                 " val xx: B = \"xxx\"/*1*/"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)

      (goto-char (ensime-test-before-label "1"))
      (ensime-assert-equal
       (ensime-implicit-notes-at (point))
       '("Implicit parameters added to call of stringToB(\"xxx\"): (zz: scala.Int)"
         "Implicit conversion of \"xxx\" using stringToB: (java.lang.String) => (scala.Int) => pack.B"))

      (ensime-test-cleanup proj))))

      (ensime-async-test
    "Test add type annotation."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "d.scala"
                     :contents ,(ensime-test-concat-lines
                                 "class D {"
                                 "  val /*2*/a = true"
                                 "  def /*3*/b = 99"
                                 "  def /*4*/test2() = 99"
                                 "  def /*5*/test3(x: Int) = 99"
                                 "  def /*6*/test4(x: Int = 99) = 99"
                                 "  def /*7*/test5(x: Int = 99)(implicit y: Int) = y"
                                 "}"))))))
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)

      (goto-char (ensime-test-after-label "7"))
      (ensime-refactor-add-type-annotation)
      (goto-char (ensime-test-after-label "6"))
      (ensime-refactor-add-type-annotation)
      (goto-char (ensime-test-after-label "5"))
      (ensime-refactor-add-type-annotation)
      (goto-char (ensime-test-after-label "4"))
      (ensime-refactor-add-type-annotation)
      (goto-char (ensime-test-after-label "3"))
      (ensime-refactor-add-type-annotation)
      (goto-char (ensime-test-after-label "2"))
      (ensime-refactor-add-type-annotation)

      (goto-char 1)
      (ensime-assert (search-forward "a: Boolean =" nil t))
      (ensime-assert (search-forward "b: Int =" nil t))
      (ensime-assert (search-forward "test2(): Int =" nil t))
      (ensime-assert (search-forward "test3(x: Int): Int =" nil t))
      (ensime-assert (search-forward "test4(x: Int = 99): Int =" nil t))
      (ensime-assert (search-forward "test5(x: Int = 99)(implicit y: Int): Int =" nil t))

      (ensime-test-cleanup proj)
      )))

   (ensime-async-test
    "Test ensime--make-result-overlay."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "sealed trait Foo"
                                 "object Foo {"
                                 ""
                                 ""
                                 ""
                                 "}"
                                 ""))))))
      (ensime-test-init-proj proj))
    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (ensime--make-result-overlay
          (format "%S" "overlay")
        :where (point-min)
        :duration 'command)
      (ensime-assert (overlayp (car(overlays-at (point-min)))))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test ensime-type-at-point overlay."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "sealed trait Foo"
                                 "object Foo {"
                                 ""
                                 "  val a /*1*/ = 1"
                                 ""
                                 "}"
                                 ""))))))
      (ensime-test-init-proj proj))
    ((:connected))
    ((:compiler-ready :full-typecheck-finished)
     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (ensime-test-eat-label "1")
      (ensime-type-at-point '(16))
      (let ((overlays (ensime-test-find-overlays-specifying 'after-string)))
        (ensime-assert (eq (length overlays) 1))
                (ensime-assert
         (string-match "Int" (overlay-get (car overlays)
                                                         'after-string))))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Test ensime-print-errors-at-point overlay."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "sealed trait Foo"
                                 "object Foo {"
                                 ""
                                 "  val a = /*1*/b"
                                 ""
                                 "}"
                                 ""))))))
      (ensime-test-init-proj proj))
    ((:connected))
    ((:compiler-ready :full-typecheck-finished :notes-added)
     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (ensime-test-eat-label "1")
      (ensime-print-errors-at-point '(16))))
    ((:errors-at-point-printed)
     (ensime-test-with-proj
      (proj src-files)
      (find-file (car src-files))
      (let ((overlays (ensime-test-find-overlays-specifying 'after-string)))
        (ensime-assert (eq (length overlays) 1))
        (ensime-assert
         (string-match "not found: value b" (overlay-get (car overlays)
                                                         'after-string))))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Ensime unit test dwim."
    (let* ((proj (ensime-create-tmp-project
		  `((:name
		     "atest/ExampleSpec.scala"
		     :relative-to "src/test/scala"
		     :contents ,(ensime-test-concat-lines
				 "package atest"
				 "import collection.mutable.Stack"
				 "import org.scalatest._"
				 "class ExampleSpec extends FlatSpec with Matchers {"

				 "  \"A Stack\" should \"pop values in last-in-first-out order\" in {"
				 "    val stack = new Stack[Int]"
				 "    stack.push(1)"
				 "    stack.push(2)"
				 "    stack.pop() should be (2)"
				 "    stack.pop() should be (1)"
				 "  }"

				 "  it should \"throw NoSuchElementException if an empty stack is popped\" in {"
				 "    val emptyStack = new Stack[Int]"
				 "    a [NoSuchElementException] should be thrownBy {"
				 "    emptyStack.pop()"
				 "    }"
				 "  }"
				 "}"))
		    (:name "build.sbt"
			   :relative-to ""
			   :contents ,(ensime-test-concat-lines
				       (concat "scalaVersion := \"" ensime--test-scala-version "\"")
                       (concat "scalaBinaryVersion := \"" (ensime--scala-binary-version ensime--test-scala-version) "\"")
				       ""
				       (concat
                        "libraryDependencies += \"org.scalatest\" % \"scalatest_"
                        (ensime--scala-binary-version ensime--test-scala-version)
                        "\" % \"2.2.4\" % \"test\""))))))
	   (src-files (plist-get proj :src-files)))
      (assert ensime-sbt-command)
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)
      (dolist
	  (tests '(("test" ensime-sbt-do-test-dwim)
		   ("testQuick" ensime-sbt-do-test-quick-dwim)
		   ("test-only atest.ExampleSpec" ensime-sbt-do-test-only-dwim)))
	(let* ((module (-> (plist-get proj :config)
			   (plist-get :subprojects)
			   -first-item
			   (plist-get :name)))
	       (command (s-concat module "/" (car tests)))
	       (f (cdr tests)))
	  (apply f)
	  (ensime-assert-equal (sbt:get-previous-command) command)))
      (ensime-test-cleanup proj))))

   (ensime-async-test
    "Ensime integration test dwim."
    (let* ((proj (ensime-create-tmp-project
		  `((:name
		     "atest/ExampleSpec.scala"
		     :relative-to "src/it/scala"
		     :contents ,(ensime-test-concat-lines
				 "package atest"
				 "import collection.mutable.Stack"
				 "import org.scalatest._"
				 "class ExampleSpec extends FlatSpec with Matchers {"

				 "  \"A Stack\" should \"pop values in last-in-first-out order\" in {"
				 "    val stack = new Stack[Int]"
				 "    stack.push(1)"
				 "    stack.push(2)"
				 "    stack.pop() should be (2)"
				 "    stack.pop() should be (1)"
				 "  }"

				 "  it should \"throw NoSuchElementException if an empty stack is popped\" in {"
				 "    val emptyStack = new Stack[Int]"
				 "    a [NoSuchElementException] should be thrownBy {"
				 "    emptyStack.pop()"
				 "    }"
				 "  }"
				 "}"))
		    (:name "build.sbt"
			   :relative-to ""
			   :contents ,(ensime-test-concat-lines
                           (concat "scalaVersion := \"" ensime--test-scala-version "\"")
                           (concat "scalaBinaryVersion := \"" (ensime--scala-binary-version ensime--test-scala-version) "\"")
				       ""
				       "lazy val root ="
				       "  Project(\"root\", file(\".\"))"
				       "  .configs( IntegrationTest )"
				       "  .settings( Defaults.itSettings : _*)"
				       "  .settings( libraryDependencies += specs )"
				       ""
				       (concat
					"lazy val specs = \"org.scalatest\" % \"scalatest_"
                    (ensime--scala-binary-version ensime--test-scala-version)
                    "\" % \"2.2.4\" % \"it\""))))
		  nil "root" '("src/it/scala")))
	   (src-files (plist-get proj :src-files)))
      (assert ensime-sbt-command)
      (ensime-test-init-proj proj))

    ((:connected))
    ((:compiler-ready :full-typecheck-finished :indexer-ready)
     (ensime-test-with-proj
      (proj src-files)
      (dolist
	  (tests '(("it:test" ensime-sbt-do-test-dwim)
		   ("it:testQuick" ensime-sbt-do-test-quick-dwim)
		   ("it:test-only atest.ExampleSpec" ensime-sbt-do-test-only-dwim)))
	(let* ((module (-> (plist-get proj :config)
			   (plist-get :subprojects)
			   -first-item
			   (plist-get :name)))
	       (command (s-concat module "/" (car tests)))
	       (f (cdr tests)))
	  (apply f)
	  (ensime-assert-equal (sbt:get-previous-command) command)))
      (ensime-test-cleanup proj))))
   ))

(defun ensime-run-all-tests ()
  "Run all regression tests for ensime-mode."
  (interactive)

  (ensime-run-suite ensime-slow-suite)

  ;; needed for -batch mode
  (while noninteractive
    (sit-for 30)))

(defun ensime-run-one-test (key)
  "Run a single test selected by title.
Must run the run-all script first to update the server."
  (interactive "sEnter a regex matching a test's title: ")
  (catch 'done
    (setq ensime--test-had-failures nil)
    (dolist (test ensime-slow-suite)
      (let ((title (plist-get test :title)))
        (when (integerp (string-match key title))
          (ensime-run-suite (list test))
          (throw 'done nil))))))

(provide 'ensime-test)

;; Local Variables:
;; End:
