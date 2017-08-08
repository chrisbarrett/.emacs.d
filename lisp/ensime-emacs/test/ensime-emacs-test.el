;;; ensime-emacs-test.el --- ensime-emacs: ERT Test helper

;; Copyright (C) 2015 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;; might be nice to put this test data in a separate file
(defconst
  ensime-test-example-type-info-1
  '(:arrow-type t
                :name "(Position, Int, Boolean) => CompletionInfoList"
                :full-name "(scala.reflect.internal.util.Position, scala.Int, scala.Boolean) => org.ensime.api.CompletionInfoList"
                :result-type (:arrow-type nil
                                          :name "CompletionInfoList"
                                          :decl-as class
                                          :full-name "org.ensime.api.CompletionInfoList")
                :param-sections ((:params (
                                           ("inputP" (:arrow-type nil :name "Position" :decl-as class :full-name "scala.reflect.internal.util.Position"))
                                           ("maxResultsArg" (:arrow-type nil :name "Int" :decl-as class :full-name "scala.Int"))
                                           ("caseSens" (:arrow-type nil :name "Boolean" :decl-as class :full-name "scala.Boolean")))))))

(defconst
  ensime-test-example-type-info-2
  '(:arrow-type t :name "(A) => StringFormat[A]" :full-name "(scala.Predef.A) => scala.Predef.StringFormat[scala.Predef.A]"
                :result-type (:arrow-type nil
                                          :name "StringFormat[A]"
                                          :decl-as class
                                          :full-name "scala.Predef.StringFormat[scala.Predef.A]"
                                          :type-args ((:arrow-type nil :name "A" :decl-as nil :full-name "scala.Predef.A")))
                :param-sections ((:params (("self" (:arrow-type nil :name "A" :decl-as nil :full-name "scala.Predef.A")))))))

(ert-deftest ensime-emacs-test-simple-config-loading ()
  (let ((conf (->> ".ensime-simple-config"
                   ensime-emacs-test-resource-filename
                   ensime-config-load)))
    (should (equal (plist-get conf :server-cmd) "bin/server.sh"))
    (should (equal (plist-get conf :dependendency-dirs) '("hello" "world")))))

(ert-deftest ensime-emacs-test-broken-config-loading ()
  (let ((conf (->> ".ensime-broken-config"
                   ensime-emacs-test-resource-filename))
        (read-error (condition-case er
                        (ensime-config-load conf)
                      (error nil))))
    (should (null read-error))))

(ert-deftest ensime-emacs-test-utf8-string-encoding-for-swank ()
  (let ((encodings '(("000001" "$" utf-8) ("000002" "£" utf-8) ("000003" "€" utf-8)
                     ("00000a" " $ £ € " utf-8)
                     ("000001" "$" nil)   ("000001" "£" nil) ("000001" "€" nil)
                     ("000007" " $ £ € " nil))))
    (-each encodings
      (-lambda ((encoded to-encode encoding))
        (should (equal encoded
                       (ensime-net-encode-length to-encode encoding)))))))

(ert-deftest ensime-emacs-test-reading-utf8-encoded-s-expression-from-swank ()
  (let ((encodings '(("000001$\n" "$") ("000002£\n" "£")
                     ("000003€\n" "€") ("00000e\"hello world!\"\n" "hello world!")
                     ("00000c( $ £ € )\n" ($ £ €)))))
    (-each encodings
      (-lambda ((utf8 res))
        (let ((eq-test (if (stringp res) 'string-equal 'equal)))
          (with-temp-buffer
            (insert utf8)
            (should (funcall eq-test res (ensime-net-read)))))))))

(ert-deftest ensime-emacs-test-name-partitioning ()
  (let ((name-partitions '(("java.util.List" ("java.util" nil "List"))
                           ("scala.tools.nsc.symtab.Types$Type" ("scala.tools.nsc.symtab" "Types" "Type"))
                           ("scala.tools.nsc.symtab.Types" ("scala.tools.nsc.symtab" nil "Types"))
                           ("scala.tools.nsc.symtab.Types$Dude$AbsType" ("scala.tools.nsc.symtab" "Types$Dude" "AbsType"))
                           ("scala.tools.nsc.symtab.Types$$Type$" ("scala.tools.nsc.symtab" "Types$" "Type$"))
                           ("Types$$Type$" ("" "Types$" "Type$")))))
    (-each name-partitions
      (-lambda ((name partition))
        (let ((result (ensime-with-name-parts name (p o n) (list p o n))))
          (should (equal partition result)))))))

(ert-deftest ensime-emacs-test-course-name-partition ()
  (let ((name-partitions '(("java.util.List" ("java.util" "List"))
                           ("scala.tools.nsc.symtab.Types" ("scala.tools.nsc.symtab" "Types"))
                           ("scala.tools.nsc.symtab.Types$Dude$AbsType" ("scala.tools.nsc.symtab" "Types$Dude$AbsType"))
                           ("scala.tools.nsc.symtab.Types$$Type$" ("scala.tools.nsc.symtab" "Types$$Type$"))
                           ("Types$$Type$" ("" "Types$$Type$"))
                           ("java.uti" ("java" "uti"))
                           ("uti" ("" "uti")))))

    (-each name-partitions
      (-lambda ((name partition))
        (let ((result (ensime-with-path-and-name name (p n) (list p n))))
          (should (equal partition result)))))))

(ert-deftest ensime-emacs-test-source-file-predicate ()
 (should (ensime-source-file-p "dude.scala"))
 (should (ensime-source-file-p "dude.java"))
 (should (not (ensime-source-file-p "dude.javap")))) 

(ert-deftest ensime-emacs-test-relativise-paths ()
  (let ((relativise '(("/home/aemon/rabbits.txt" "/home/aemon/" "./rabbits.txt")
                      ("/home/aemon/a/b/d.txt" "/home/aemon/" "./a/b/d.txt")
                      ("c:/home/aemon/a/b/d.txt" "c:/home/aemon/" "./a/b/d.txt")
                      ("c:/home/blamon/a/b/d.txt" "c:/home/aemon/" "c:/home/blamon/a/b/d.txt"))))
    (-each relativise
      (-lambda ((full-path rel-to rel-path))
        (should (equal rel-path (ensime-relativise-path full-path rel-to))))))) 

(ert-deftest ensime-emacs-test-short-local-name ()
  (let ((short-locals '(("Junk" "Junk")
                        ("Foo$$Junk" "Junk")
                        ("Foo$$Junk$" "Junk"))))
    (-each short-locals
      (-lambda ((full short))
        (should (equal short (ensime-short-local-name full)))))))

(ert-deftest ensime-emacs-test-strip-dollar-signs ()
  (let ((strip-dollars '(("com.example.Foo$" "com.example.Foo")
                         ("com.example.Foo$$Junk" "com.example.Foo.Junk"))))
    (-each strip-dollars
      (-lambda ((dollars stripped))
        (should (equal stripped (ensime-strip-dollar-signs dollars)))))))

(ert-deftest ensime-emacs-test-replace-keywords ()
  (should
   (equal
    '("str1" "foo" "str2" "str3" "a" "b" "c" "bar")
    (ensime-replace-keywords '("str1" :key1 "str2" "str3" :key2 :key3)
                             '(:key1 "foo" :key2 ("a" "b" "c") :key3 "bar")))))

(ert-deftest ensime-emacs-test-stacktrace-groups-lines-to-fold ()
 (let ((grouped-lines (ensime-stacktrace-group-lines-to-fold '(10 9 8 6 5 3 1))))
   (should (equal grouped-lines '((1) (3) (5 6) (8 9 10))))))

(ert-deftest ensime-emacs-test-stacktrace-pick-lines-to-fold ()
    (with-temp-buffer
      (insert (concat "java.util.NoSuchElementException: None.get\n"
                      "\tat scala.None$.get(Option.scala:347)\n"
                      "\tat scala.None$.get(Option.scala:345)\n"
                      "\tat akka.actor.ActorCell.invoke(ActorCell.scala:487)\n"
                      "\tat akka.dispatch.Mailbox.processMailbox(Mailbox.scala:254)\n"
                      "\tat akka.dispatch.Mailbox.run(Mailbox.scala:221)\n"
                      "\tat akka.dispatch.Mailbox.exec(Mailbox.scala:231)\n"
                      "\tat scala.concurrent.forkjoin.ForkJoinTask.doExec(ForkJoinTask.java:260)\n"
                      "\tat scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.pollAndExecAll(ForkJoinPool.java:1253)\n"
                      "\tat scala.concurrent.forkjoin.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1346)\n"
                      "\tat scala.concurrent.forkjoin.ForkJoinPool.runWorker(ForkJoinPool.java:1979)\n"))
      (let ((lines-to-fold (ensime-stacktrace-pick-lines-to-fold '("at akka\\.*"))))
        (should (equal lines-to-fold '(7 6 5 4))))))

(ert-deftest ensime-emacs-test-sem-high-internalize-syms-unix ()
 (with-temp-buffer
   (let* ((contents "a\nbc\nd\nef\ngh")
          (num-chars (length contents))
          (last-offset num-chars)
          syms
          internalized-syms
          expected)
     (insert contents)
     (dotimes (i last-offset)
       (push (list 'a i last-offset) syms)
       (push (list 'a
                   (ensime-internalize-offset i)
                   (ensime-internalize-offset last-offset))
             expected))
     (setf expected (sort expected (lambda (a b) (< (nth 1 a) (nth 1 b)))))
     (setf internalized-syms
           (sort (ensime-sem-high-internalize-syms syms)
                 (lambda (a b) (< (nth 1 a) (nth 1 b)))))
     (should (equal internalized-syms expected)))))

(ert-deftest ensime-emacs-test-sem-high-internalize-syms-dos ()
 (with-temp-buffer
   (setf buffer-file-coding-system 'undecided-dos)
   (let* ((contents "a\nbc\nd\nef\ngh")
          (num-chars (length contents))
          (last-offset (+ 5 num-chars))
          syms
          internalized-syms
          expected)
     (insert contents)
     (dotimes (i last-offset)
       (push (list 'a i last-offset) syms)
       (push (list 'a
                   (ensime-internalize-offset i)
                   (ensime-internalize-offset last-offset))
             expected))
     (setf expected (sort expected (lambda (a b) (< (nth 1 a) (nth 1 b)))))
     (setf internalized-syms
           (sort (ensime-sem-high-internalize-syms syms)
                 (lambda (a b) (< (nth 1 a) (nth 1 b)))))
     (should (equal internalized-syms expected)))))

(ert-deftest ensime-emacs-test-path-includes-dir-p ()
  (unless (member system-type '(windows-nt cygwin))
      (let ((d (make-temp-file "ensime_test_proj" t)))
        (make-directory (concat d "/proj/src/main") t)
        (make-directory (concat d "/proj/src/main/java") t)
        (ensime-create-file (concat d "/proj/src/main/java/Test.java") "import java.util.bla")
        (make-directory (concat d "/tmp/scala_misc") t)
        (ensime-create-file (concat d "/tmp/scala_misc/Test.scala") "import java.util.bla")
        (ensime-create-file (concat d "/tmp/other_misc/Things.scala") "import bla bla")
        (make-symbolic-link (concat d "/tmp/scala_misc") (concat d "/proj/src/main/scala"))
        (make-symbolic-link (concat d "/tmp/other_misc/Things.scala") (concat d "/proj/src/main/scala/Things.scala"))
        (should (file-exists-p (concat d "/proj/src/main/java/Test.java")))
        (should (file-exists-p (concat d "/proj/")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/java/Test.java")
                                            (concat d "/proj")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Test.scala")
                                            (concat d "/proj")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Test.scala")
                                            (concat d "/proj/src")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Test.scala")
                                            (concat d "/proj/src/main")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Test.scala")
                                            (concat d "/")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Things.scala")
                                            (concat d "/proj/src/main/scala")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Things.scala")
                                            (concat d "/proj/src/")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Things.scala")
                                            (concat d "/proj/src")))
        (should (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Things.scala")
                                            (concat d "/tmp/scala_misc")))
        (should (not (ensime-path-includes-dir-p (concat d "/proj/src/main/scala/Things.scala")
                                                 (concat d "/proj/x"))))
        ;; intentionally not in an unwind-protect so it exists on failure
        (delete-directory d t))))

(ert-deftest ensime-emacs-test-inf-repl-config ()
 (let ((test-config
        '(:scala-version "test-inf-repl-config"
                         :java-home "/x/y/jdk" :targets ("a") :compile-deps ("b" "c") :runtime-deps ("d" "e")
                         :java-flags ("flag1" "flag2")
                         :scala-compiler-jars ("/x/y/scala-compiler-2.11.5.jar"
                                               "/x/y/something-else-1.2.jar"
                                               "/x/y/scala-reflect-2.11.5.jar")
                         :subprojects
                         ((:targets ("f") :compile-deps ("g") :runtime-deps ("h"))
                          (:targets ("i") :compile-deps ("j" "k") :runtime-deps ("l" "m"))))))
   (should (equal (ensime-inf-repl-config test-config)
                  `(:java "/x/y/jdk/bin/java"
                          :java-flags ("flag1" "flag2")
                          :classpath "/x/y/scala-compiler-2.11.5.jar:/x/y/something-else-1.2.jar:/x/y/scala-reflect-2.11.5.jar:a:b:c:d:e:f:g:h:i:j:k:l:m")))))

(ert-deftest support-scala-versions ()
  (should (equal (ensime--scala-binary-version "2.10.6")
                 "2.10"))
  (should (equal (ensime--scala-binary-version "2.11.8")
                 "2.11"))
  (should (equal (ensime--scala-binary-version "2.11.8-tl-201604131941")
                 "2.11"))
  )

(ert-deftest ensime-param-section-accepts-block-p-test ()
  (should (equal t
                 (ensime-param-section-accepts-block-p
                  '(:params (("block" (:arrow-type t
                                                   :name "(Int) => String"
                                                   :full-name "(scala.Int) => java.lang.String"
                                                   :result-type (:arrow-type nil
                                                                             :name "String"
                                                                             :decl-as class
                                                                             :full-name "java.lang.String")
                                                   :param-sections ((:params (("_0" (:arrow-type nil
                                                                                                 :name "Int"
                                                                                                 :decl-as class
                                                                                                 :full-name "scala.Int"))))))))))))
  )

(ert-deftest ensime-flatten-structure-view--incomplete ()
  (should (not
           (ensime-flatten-structure-view
            '(:keyword "def" :name "log" :position (:type empty))
            nil "MyThing"))))

(require 'ensime-company)
(ert-deftest ensime--build-yasnippet-for-call-test ()
  (setq ensime-test-data
        '((:params (("block" (:arrow-type t
                                          :name "(Int) => String" :full-name "(scala.Int) => java.lang.String"
                                          :result-type (:arrow-type nil :name "String" :decl-as class :full-name "java.lang.String")
                                          :param-sections ((:params (("_0" (:arrow-type nil :name "Int" :decl-as class :full-name "scala.Int")))))))))))

  (should (equal " { ${1:_0: Int} => ${2:String} }$0"
                 (ensime--build-yasnippet-for-call ensime-test-data nil t)))

  (should (equal "(${1:block: (Int) => String})"
                 (ensime--build-yasnippet-for-call ensime-test-data nil nil)))

  )
