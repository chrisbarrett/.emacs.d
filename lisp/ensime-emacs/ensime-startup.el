;;; ensime-startup.el --- download and launch ENSIME server

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'dash)
(require 's)

(defvar ensime-idle-typecheck-timer nil
  "Timer called when emacs is idle")

(defvar ensime-last-change-time 0
  "Time of last buffer change")

(defvar ensime-server-process-start-hook nil
  "Hook called whenever a new process gets started.")

(defvar ensime--classpath-separator
  (if (find system-type '(cygwin windows-nt)) ";" ":")
  "Separator used in Java classpaths")

(defvar ensime--abort-connection nil)

(defvar ensime--debug-messages nil
  "When true, show debugging information in the echo area")

(defvar ensime-startup-dirname (expand-file-name "ensime" user-emacs-directory)
  "Directory where the classfile or assembly jars are stored.")

(defvar ensime-startup-snapshot-notification t
  "Show a warning about using rolling release.
It is important that users know what they are getting into.")

(defun ensime-startup-notifications ()
  "Invasive informational messages that users need to be aware of."

  (when (and ensime-startup-snapshot-notification
             (s-contains? "SNAPSHOT" ensime-server-version))
    (let ((developer (generate-new-buffer "*ENSIME Developer Edition*")))
      (with-current-buffer developer
        (insert
         "You are tracking a SNAPSHOT version of ensime-server.
If you did not intentionally do this, it means that you are using
the unstable (developer) release of ensime (e.g. from MELPA),
which is *not recommended*.

To install the stable release of ensime, delete
~/.emacs.d/elpa/ensime and follow the instructions at:

* http://ensime.org/editors/emacs/install/


If, however, you are happy to continue tracking the developer
edition of ENSIME:

1. you are expected to remain up-to-date with developments. You
   will get access to new features but regressions in existing
   features will happen from time to time (typically while we
   await a volunteer to make required changes to the emacs
   plugin, please help by getting involved!).

2. remember to upgrade both the emacs and server components
   regularly as they can get out of sync with each other. Do not
   forget to follow the process outlined in
   http://ensime.org/editors/emacs/troubleshooting/

Please get involved in the development of ensime and help to
create high quality reproductions of bugs.

You can disable this message permanently by setting
`ensime-startup-snapshot-notification' to `nil', acknowledging
that you have read this message.")
        (read-only-mode t)
        (display-buffer developer #'display-buffer-pop-up-window))))

  ;; welcome is more important, make sure it wins the popup race
  (unless (file-exists-p ensime-startup-dirname)
    (let ((welcome (generate-new-buffer "*ENSIME Welcome*")))
      (with-current-buffer welcome
        (insert "It looks like you've just installed ENSIME, welcome!

ENSIME is more complex than a typical Emacs plugin and interacts
with an external java application (which will be downloaded and
started automatically).

You are strongly recommended to read the documentation at

* http://ensime.org/getting_started/
* http://ensime.org/editors/emacs/
* http://ensime.org/getting_help/

before proceeding.

We are all volunteers with our own priorities to improve ENSIME.
Our approach to feature requests is that we will enthusiastically
help you to implement or fix it. The ENSIME codebase is
surprisingly easy to understand and you are invited to read the
contributing guide and jump in: http://ensime.org/contributing/

This notification will only appear if you do not have a directory
at `ensime-startup-dirname'.")
        (read-only-mode t)
        (display-buffer welcome #'display-buffer-pop-up-window)))))


(defconst ensime--sbt-start-template
"
import sbt._
import IO._
import java.io._

scalaVersion := \"_scala_version_\"
scalaBinaryVersion := \"_scala_binary_version_\"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

// allows local builds of scala
resolvers += Resolver.mavenLocal

// this is where the ensime-server snapshots are hosted
resolvers += Resolver.sonatypeRepo(\"snapshots\")

libraryDependencies += \"org.ensime\" %% \"ensime\" % \"_server_version_\"

dependencyOverrides ++= Set(
  \"org.scala-lang\" % \"scala-compiler\" % scalaVersion.value,
  \"org.scala-lang\" % \"scala-library\" % scalaVersion.value,
  \"org.scala-lang\" % \"scala-reflect\" % scalaVersion.value,
  \"org.scala-lang\" % \"scalap\" % scalaVersion.value
)

val saveClasspathTask = TaskKey[Unit](\"saveClasspath\", \"Save the classpath to a file\")

saveClasspathTask := {
  val managed = (managedClasspath in Runtime).value.map(_.data.getAbsolutePath)
  val unmanaged = (unmanagedClasspath in Runtime).value.map(_.data.getAbsolutePath)
  val out = file(\"_classpath_file_\")
  write(out, (unmanaged ++ managed).mkString(File.pathSeparator))
}
")

(defun ensime-server-update ()
  "Install the most recent version of ENSIME server."
  (interactive)
    (let* ((config-file (ensime-config-find))
           (config (ensime-config-load config-file))
           (scala-version (plist-get config :scala-version)))
      (ensime--update-server scala-version `(lambda () (message "ENSIME server updated.")))))

(defun ensime--maybe-update-and-start (orig-buffer-file-name &optional host port)
  (let* ((config-file (ensime-config-find orig-buffer-file-name))
         (config (ensime-config-load config-file)))
    (if (and host port)
        ;; When both host and port are provided, we assume we're connecting to
        ;; an existing, listening server.
        (let ((cache-dir (file-name-as-directory (ensime--get-cache-dir config))))
          (ensime--retry-connect nil host (lambda () port) config cache-dir))
      (let* ((scala-version (plist-get config :scala-version))
             (assembly-file (ensime-startup-assembly-filename scala-version))
             (classpath-file (ensime-startup-classpath-filename scala-version)))
        (if (and (not (file-exists-p assembly-file))
                 (ensime--classfile-needs-refresh-p classpath-file))
            (ensime--update-server scala-version `(lambda () (ensime--1 ,config-file)))
          (ensime--1 config-file))))))

(defun ensime--maybe-update-and-start-noninteractive (orig-buffer-file-name)
  (let ((ensime-prefer-noninteractive t))
    (ensime--maybe-update-and-start orig-buffer-file-name)))

(defun* ensime--1 (config-file)
  (when (and (ensime-source-file-p) (not ensime-mode))
    (ensime-mode 1))
  (let* ((config (ensime-config-load config-file))
         (root-dir (ensime--get-root-dir config) )
         (cache-dir (file-name-as-directory (ensime--get-cache-dir config)))
         (name (ensime--get-name config))
         (scala-version (plist-get config :scala-version))
         (server-env (or (plist-get config :server-env) ensime-default-server-env))
         (buffer (or (plist-get config :buffer) (concat ensime-default-buffer-prefix name)))
         (server-java (file-name-as-directory (ensime--get-java-home config)))
         (server-flags (or (plist-get config :java-flags) ensime-default-java-flags)))
    (make-directory cache-dir 't)

    (let* ((server-proc
            (ensime--maybe-start-server
             (generate-new-buffer-name (concat "*" buffer "*"))
             server-java scala-version server-flags
             (list* (concat "JAVA_HOME=" server-java)
                    server-env)
             config-file
             cache-dir))
           (host "127.0.0.1")
           (port-fn (lambda () (ensime--read-portfile
                             (concat cache-dir "/port")))))

      ;; Store the config on the server process so we can identify it later.
      (process-put server-proc :ensime-config config)
      (push server-proc ensime-server-processes)
      (ensime--retry-connect server-proc host port-fn config cache-dir))))


;; typecheck continually when idle

(defun ensime-idle-typecheck-set-timer ()
  (when (timerp ensime-idle-typecheck-timer)
    (cancel-timer ensime-idle-typecheck-timer))
  (setq ensime-idle-typecheck-timer
        (run-with-timer nil
                        ensime-typecheck-idle-interval
                        'ensime-idle-typecheck-function)))

(defun ensime-after-change-function (start stop len)
  (set (make-local-variable 'ensime-last-change-time) (float-time)))

(defun ensime-idle-typecheck-function ()
  (when (and ensime-typecheck-when-idle
             (ensime-connected-p)
             (ensime-analyzer-ready))
    (let* ((now (float-time))
           (last-typecheck (ensime-last-typecheck-run-time (ensime-connection)))
           (earliest-allowed-typecheck (+ last-typecheck ensime-typecheck-interval)))
      (when (and (>= now (+ ensime-last-change-time ensime-typecheck-idle-interval))
                 (>= now earliest-allowed-typecheck)
                 (< last-typecheck ensime-last-change-time))
        (ensime-typecheck-current-buffer)
        (ensime-sem-high-refresh-hook)))))

(defun ensime-reload ()
  "Re-initialize the project with the current state of the config file.
Analyzer will be restarted."
  (interactive)
  (ensime-shutdown)
  (ensime))

(defun ensime--maybe-start-server (buffer java-home scala-version flags env config-file cache-dir)
  "Return a new or existing server process."
  (let ((existing (comint-check-proc buffer)))
    (if existing existing
      (ensime--start-server buffer java-home scala-version flags env config-file cache-dir))))

(defun ensime-startup-assembly-filename (scala-version)
  "The filename of an assembly jar for SCALA-VERSION.
If such a file is present, it will override the `ensime-startup-classpath-filename'.
Assembly jars are available at http://ensime.typelevel.org"
  (expand-file-name
   (format "ensime_%s-%s-assembly.jar" (ensime--scala-binary-version scala-version) ensime-server-version)
   ensime-startup-dirname))

(defun ensime-startup-classpath-filename (scala-version)
  "The filename containing the ensime-server classpath for SCALA-VERSION."
  (expand-file-name
   (format "classpath_%s_%s" scala-version ensime-server-version)
   ensime-startup-dirname))

(defun ensime--classfile-needs-refresh-p (classfile)
  "Do we need to update the CLASSFILE?"
  (or (not (file-exists-p classfile))
      (when (s-contains? "SNAPSHOT" ensime-server-version)
        (let ((ensime-el (locate-file "ensime" load-path '(".el" ".elc"))))
          (when ensime-el
            (ensime--dependencies-newer-than-target-p
             classfile
             (directory-files (file-name-directory ensime-el) t "^ensime.*\\.elc?$")))))))

(defun ensime--update-sentinel (process event scala-version on-success-fn)
  (cond
   ((equal event "finished\n")
    (let ((classpath-file (ensime-startup-classpath-filename scala-version)))
      (if (file-exists-p classpath-file)
          (funcall on-success-fn)
        (message "Could not create classpath file %s" classpath-file))))
   (t
    (message "Process %s exited: %s" process event))))

(defun ensime--update-server (scala-version on-success-fn)
  (with-current-buffer (get-buffer-create "*ensime-update*")
    (erase-buffer)
    (let* ((default-directory (file-name-as-directory
                               (make-temp-file "ensime_update_" t)))
           (classpath-file (ensime-startup-classpath-filename scala-version))
           (buildfile (expand-file-name "build.sbt"))
           (buildcontents (ensime--create-sbt-start-script scala-version))
           (buildpropsfile (expand-file-name "project/build.properties")))

      (when (file-exists-p classpath-file) (delete-file classpath-file))
      (make-directory (file-name-directory classpath-file) t)
      (ensime-write-to-file buildfile buildcontents)
      (ensime-write-to-file buildpropsfile "sbt.version=0.13.11\n")

      (if (executable-find ensime-sbt-command)
          (let ((process (start-process "*ensime-update*" (current-buffer)
                                        ensime-sbt-command "saveClasspath" "clean")))
            (set-process-sentinel process
                                  `(lambda (process event)
                                     (ensime--update-sentinel process
                                                              event
                                                              ,scala-version
                                                              ',on-success-fn)))
            (message "Updating ENSIME server..."))
        (error "sbt command not found")))))

(defun ensime--monkeys-first (classpath)
  "The ensime-monkeys jar must appear ahead at the head of the classpath."
  (sort (copy-sequence classpath)
        (lambda (a b)
          (cond
           ((string-match ".*monkeys.*" a) t)
           (t nil)))))

(defun ensime--start-server (buffer java-home scala-version flags user-env config-file cache-dir)
  "Start an ensime server in the given buffer and return the created process.
BUFFER is the buffer to receive the server output.
FLAGS is a list of JVM flags.
USER-ENV is a list of environment variables.
CACHE-DIR is the server's persistent output directory."
  (message "ENSIME server starting...")
  (with-current-buffer (get-buffer-create buffer)
    (comint-mode)
    (let* ((default-directory cache-dir)
           (tools-jar (expand-file-name "lib/tools.jar" java-home))
           (assembly-file (ensime-startup-assembly-filename scala-version))
           (classpath-file (ensime-startup-classpath-filename scala-version))
           (scala-compiler-jars (plist-get config :scala-compiler-jars))
           (server-classpath (if (file-exists-p assembly-file)
                                 (cons assembly-file scala-compiler-jars)
                               (ensime--monkeys-first
                                (s-split ensime--classpath-separator
                                         (ensime-read-from-file classpath-file)))))
           (classpath (s-join ensime--classpath-separator
                              (append server-classpath (list tools-jar))))
           (process-environment (append user-env process-environment))
           (java-command (expand-file-name "bin/java" java-home))
           (args (-flatten (list
                            "-classpath" classpath
                            flags
                            (concat "-Densime.config=" (expand-file-name config-file))
                            (when ensime-server-logback
                              (concat "-Dlogback.configurationFile=" ensime-server-logback))
                            "org.ensime.server.Server"))))

      (set (make-local-variable 'comint-process-echoes) nil)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)

      ;; Get rid of default filters including ansi coloring, scroll to bottom,
      ;; and scanning for password prompt. These use non-trivial cpu.
      (set (make-local-variable 'comint-output-filter-functions) nil)

      (when ensime--debug-messages
        (make-local-variable 'comint-output-filter-functions)
        (push #'(lambda (str) (message "%s" str)) comint-output-filter-functions))

      (insert (format "Starting ENSIME server: %s %s\n"
                      java-command
                      (mapconcat 'identity args " ")))
      (if (executable-find java-command)
          (comint-exec (current-buffer) buffer java-command nil args)
        (error "java command %s not found" java-command))
      ;; Make sure we clean up nicely (required on Windows, or port files won't
      ;; be removed).
      (add-hook 'kill-emacs-hook 'ensime-kill-emacs-hook-function)
      (add-hook 'kill-buffer-hook 'ensime-interrupt-buffer-process nil t)
      (let ((proc (get-buffer-process (current-buffer))))
        (ensime-set-query-on-exit-flag proc)
        (run-hooks 'ensime-server-process-start-hook)
        proc))))

(defun ensime-kill-emacs-hook-function ()
  "Swallow and log errors on exit."
  (condition-case err
      (ensime-interrupt-all-servers)
    (message "Error while killing emacs: %s" err)))

(defun ensime--scala-binary-version (full-version)
  "The scala binary version given a full version string."
  (pcase (version-to-list (car (s-split "-" full-version)))
    (`(2 10 ,_) "2.10")
    (`(2 11 ,_) "2.11")
    (t (error "unsupported scala version %s" full-version))))

(defun ensime--create-sbt-start-script (scala-version)
  ;; emacs has some weird case-preservation rules in regexp replace
  ;; see http://github.com/magnars/s.el/issues/62
  (s-replace-all (list (cons "_scala_version_" scala-version)
                       (cons "_scala_binary_version_" (ensime--scala-binary-version scala-version))
                       (cons "_server_version_" ensime-server-version)
                       (cons "_classpath_file_" (ensime-startup-classpath-filename scala-version)))
                 ensime--sbt-start-template))


(defun ensime-shutdown ()
  "Terminate the associated ENSIME server (equivalent to killing its buffer)."
  (interactive)
  (let* ((config (ensime-config-for-buffer))
         (server-process (and config (ensime-process-for-config config))))
    (if (not server-process)
        (error "Couldn't find the ENSIME server for this buffer.")
      (kill-buffer (process-buffer server-process)))))

(defun ensime-configured-project-root ()
  "Return root path of the current project as defined in the
config file and stored in the current connection. Nil is returned
if there is no active connection, or if the project root was not
defined."
  (when (ensime-connected-p)
    (let ((config (ensime-config (ensime-connection))))
      (plist-get config :root-dir))))

(defun ensime--read-portfile (portfile)
  "Read the contents of PORTFILE."
  (when (file-exists-p portfile)
    (save-excursion
      (with-temp-buffer
        (insert-file-contents portfile)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun ensime--retry-connect (server-proc host port-fn config cache-dir)
  "When application of port-fn yields a valid port, connect to the port at the
 given host. Otherwise, schedule ensime--retry-connect for re-execution after 5
 seconds."
  (cond (ensime--abort-connection
         (setq ensime--abort-connection nil)
         (message "Aborted"))
        ((and server-proc (eq (process-status server-proc) 'exit))
         (message "Failed to connect: server process exited."))
        (t
         (let ((port (funcall port-fn)))
           (unless (and port
                        (condition-case nil
                            (progn
                              (ensime--connect host port config)
                              t)
                          (error
                           (not (message "failed to connect to port %s, trying again..." port)))))
             (run-at-time
              "5 sec" nil 'ensime-timer-call 'ensime--retry-connect
              server-proc host port-fn config cache-dir))))))

(defun ensime--connect (host port config)
  (let ((c (ensime-connect host port)))
    (ensime-set-config c config)
    (let ((ensime-dispatching-connection c))
      (ensime-eval-async
       ;; hmm, this poses a problem... we can't really do anything
       ;; until we know the protocol version. This shouldn't be async,
       ;; but making it sync gives us "error in timer" errors.
       '(swank:connection-info)
       'ensime-handle-connection-info))))

(defun ensime-timer-call (fun &rest args)
  "Call function FUN with ARGS, reporting all errors.
   The default condition handler for timer functions (see
   `timer-event-handler') ignores errors."
  (condition-case data
      (apply fun args)
    (error (debug nil (list "Error in timer" fun args data)))))

(defun ensime--abort-connection ()
  "Abort connection the current connection attempt."
  (interactive)
  (setq ensime--abort-connection 't))


(provide 'ensime-startup)

;; Local Variables:
;; End:
