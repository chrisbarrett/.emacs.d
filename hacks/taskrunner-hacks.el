;;; taskrunner-hacks.el --- Hacks for taskrunner.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)
(require 'paths)

(el-patch-feature taskrunner)

(defconst taskrunner-hacks--cache-file (f-join paths-cache-directory "taskrunner" "taskrunner-tasks.eld"))

(with-eval-after-load 'taskrunner
  (with-no-warnings
    (el-patch-defun taskrunner-read-cache-file ()
      "Read the task cache file and initialize the task caches with its contents."
      (with-temp-buffer
        (let ((taskrunner-cache-filepath (el-patch-swap (expand-file-name "taskrunner-tasks.eld" user-emacs-directory)
                                                        taskrunner-hacks--cache-file))
              (file-tasks))
          (when (file-exists-p taskrunner-cache-filepath)
            (with-temp-buffer
              (insert-file-contents taskrunner-cache-filepath)
              (setq file-tasks (car (read-from-string (buffer-string))))
              ;; Load all the caches with the retrieved info
              (setq taskrunner-tasks-cache (nth 0 file-tasks))
              (setq taskrunner-last-command-cache(nth 1 file-tasks))
              (setq taskrunner-build-cache (nth 2 file-tasks))
              (setq taskrunner-command-history-cache (nth 3 file-tasks))
              ;; Length is checked for backwards compatibility.  The cache file will
              ;; be overwritten soon but if the user installed this package before
              ;; the new cache was added, trying to read in the new command cache
              ;; will throw an error
              (when (= (length file-tasks) 5)
                (setq taskrunner-custom-command-cache (nth 4 file-tasks))))))))

    (el-patch-defun taskrunner-write-cache-file ()
      "Save all tasks in the cache to the cache file in Emacs user directory."
      (let ((taskrunner-cache-filepath (el-patch-swap (expand-file-name "taskrunner-tasks.eld" user-emacs-directory)
                                                      taskrunner-hacks--cache-file)))
        (write-region (format "%s%s\n" taskrunner--cache-file-header-warning
                              (list (prin1-to-string taskrunner-tasks-cache)
                                    (prin1-to-string taskrunner-last-command-cache)
                                    (prin1-to-string taskrunner-build-cache)
                                    (prin1-to-string taskrunner-command-history-cache)
                                    (prin1-to-string taskrunner-custom-command-cache)))
                      nil
                      taskrunner-cache-filepath)))

    (el-patch-defun taskrunner-delete-cache-file ()
      "Delete the cache file used for persistence between Emacs sessions.
The user will be asked to confirm this action before deleting the file."
      (if (y-or-n-p "Are you sure you want to delete the cache file? ")
          (delete-file (el-patch-swap (expand-file-name "taskrunner-tasks.eld" user-emacs-directory)
                                      taskrunner-hacks--cache-file))))))

(provide 'taskrunner-hacks)

;;; taskrunner-hacks.el ends here
