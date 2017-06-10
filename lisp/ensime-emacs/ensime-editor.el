;;; ensime-editor.el -- Editor and navigation commands -*- lexical-binding: t -*-

;; Copyright (C) 2015 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'dash)
(require 'popup)
(require 'ensime-vars)

(autoload 'ensime-helm-select-entry "ensime-helm")

(defvar ensime-compile-result-buffer-name "*ENSIME-Compilation-Result*")

(defvar ensime-compile-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'ensime-show-all-errors-and-warnings)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the build result popup.")

(defface ensime-compile-warnline
  '((t (:inherit compilation-warning)))
  "Face used for marking the line on which an warning occurs."
  :group 'ensime-ui)

(defface ensime-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'ensime-ui)

(defface ensime-compile-infoline
  '((t (:inherit compilation-info)))
  "Face used for marking a line on which there is information available."
  :group 'ensime-ui)

(defvar ensime-selection-overlay nil)

(defvar ensime-selection-stack nil)

(defvar ensime-ui-method-bytecode-handler
  (list
   :init (lambda (info)
	   (ensime-ui-insert-method-bytecode info))
   :update (lambda (info))
   :help-text "Press q to quit."
   :writable nil
   :keymap `()))

(defvar ensime-uses-buffer-name "*Uses*")

(defvar ensime-uses-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    map)
  "Key bindings for the uses popup.")



(defun ensime-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ensime-line-col-to-point (file line col)
  "Convert line,column coordinates to a char offset."
  (with-temp-buffer
    (insert-file-contents file)
    (ensime-goto-line line)
    (forward-char col)
    (point)))

(defun ensime-current-line ()
  "Return the vertical position of point..."
  (1+ (count-lines 1 (point))))

;; Displaying proposed changes

(defun ensime-insert-change-list (changes)
  "Describe a series of proposed file changes. Used for
 refactoring and undo confirmation buffers."
  (let ((grouped-changed
	 (ensime-group-changes-by-proximity changes)))
    (dolist (ch grouped-changed)
      (let* ((file (plist-get ch :file))
	     (text (plist-get ch :text))
	     (range-start (ensime-internalize-offset-for-file
			   file (plist-get ch :from)))
	     (range-end (ensime-internalize-offset-for-file
			 file (plist-get ch :to)))
	     (edits (plist-get ch :edits)))


	;; Make sure edits is not empty
	(when edits

	  (let* ((edits (copy-list edits));; So we can destructively modify
		 (result (ensime-extract-file-chunk
			  file (- range-start 150) (+ range-end 150)))
		 (chunk-text (plist-get result :text))
		 (chunk-coding-system (plist-get result :chunk-coding-system))
		 (chunk-start (plist-get result :chunk-start))
		 (chunk-end (plist-get result :chunk-end))
		 (chunk-start-line (plist-get result :chunk-start-line)))


	    ;; Sort in reverse textual order
	    ;; so we can apply edits without disturbing
	    ;; positions further down in chunk.
	    (setq edits (sort edits
			      (lambda (a b)
				(> (plist-get a :from)
				   (plist-get b :from)))))

	    ;; Insert heading for chunk

	    (ensime-insert-with-face file 'font-lock-comment-face)
	    (ensime-insert-with-face
	     (format "\n------------------- @line %s -----------------------\n"
		     chunk-start-line)
	     'font-lock-comment-face)

	    (let ((p (point)))
	      (insert chunk-text)

	      ;; Highlight all the edits in the chunk

	      (dolist (ed edits)
		(let* ((text (plist-get ed :text))
		       (from (ensime-internalize-offset-for-file file (plist-get ed :from)))
		       (to (ensime-internalize-offset-for-file file (plist-get ed :to)))
		       (len (- to from)))
		  (goto-char (+ p (- from chunk-start)))
		  (delete-char (min len (- (point-max) (point))))

                  (when (eq 1 (coding-system-eol-type chunk-coding-system))
                    (setq text (replace-regexp-in-string "\r$" "" text)))

                  (let ((start (point)))
                    (insert text)
                    (set-text-properties start (point) '(face font-lock-keyword-face)))))

	      (goto-char (point-max))
	      (insert "\n\n\n"))))))))


(defun ensime-changes-are-proximate-p (ch1 ch2)
  "Return t if ch1 and ch2 occur nearby in the same file."
  (let* ((len1 (- (plist-get ch1 :to)
		  (plist-get ch1 :from)))
	 (mid1 (+ (plist-get ch1 :from) (/ len1 2)))
	 (len2 (- (plist-get ch2 :to)
		  (plist-get ch2 :from)))
	 (mid2 (+ (plist-get ch2 :from) (/ len2 2))))

    (and (equal (plist-get ch1 :file )
		(plist-get ch2 :file ))
	 (< (abs (- mid1 mid2)) 1000))))


(defun ensime-merge-changes (changes)
  "Return a single change with edits that correspond
 to all edits in all elements of changes."
  (let ((range-start most-positive-fixnum)
	(range-end most-negative-fixnum)
	(edits '())
	(file nil))

    (dolist (ch changes)
      (let ((from (plist-get ch :from))
	    (to (plist-get ch :to)))
	(setq range-start (min range-start from))
	(setq range-end (max range-end to))
	(setq edits (append (plist-get ch :edits)
			    edits))))
    (list
     :file (plist-get (first changes) :file)
     :from range-start
     :to range-end
     :edits edits)))


(defun ensime-group-changes-by-proximity (changes)
  "Create aggregate changes for changes that occur nearby
 eachother in the same file."
  (let ((changes
	 (mapcar
	  (lambda (ch)
	    (list
	     :file (plist-get ch :file)
	     :from (plist-get ch :from)
	     :to (plist-get ch :to)
	     :edits (list
		     (list
		      :from (plist-get ch :from)
		      :to (plist-get ch :to)
		      :text (plist-get ch :text)))))
	  changes))
	(merged '()))

    (while changes
      (let ((ch (pop changes))
	    (neighbors '())
	    (update-merged '()))

	(dolist (m merged)
	  (if (ensime-changes-are-proximate-p m ch)
	      (push m neighbors)
	    (push m update-merged)))

	(push (ensime-merge-changes (cons ch neighbors))
	      update-merged)

	(setq merged update-merged)))

    ;; Sort in textual order
    (sort merged (lambda (a b)
		   (< (plist-get a :from)
		      (plist-get b :from))))))


(defun ensime-extract-file-chunk (file-name start end)
  "Return the text of the given file from start to end."
  (with-temp-buffer
    (insert-file-contents file-name)
    (let* ((coding-system last-coding-system-used)
           (chunk-start
            (progn
              (goto-char start)
              (point-at-bol)))
	   (chunk-end
            (progn
              (goto-char end)
              (point-at-eol)))
	   (text (buffer-substring-no-properties chunk-start chunk-end)))
      (list :text text
            :chunk-coding-system coding-system
	    :chunk-start chunk-start
	    :chunk-end chunk-end
	    :chunk-start-line (line-number-at-pos chunk-start)))))



;; Jump to definition


(defun ensime-push-definition-stack ()
  "Add point to find-tag-marker-ring."
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))

(defun ensime-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (pop-tag-mark))

(defun ensime-edit-definition-other-window ()
  (interactive)
  (ensime-edit-definition 'window))

(defun ensime-edit-definition-other-frame ()
  (interactive)
  (ensime-edit-definition 'frame))

(defun ensime-edit-definition (&optional where)
  "Lookup the definition of the name at point.
Goes to the point of the definition (returning point), or fails with `nil'."
  (interactive)
  (let* ((info (ensime-rpc-symbol-at-point))
         (pos (ensime-symbol-decl-pos info)))
    (if (ensime-pos-valid-local-p pos)
        (progn
          (ensime-push-definition-stack)
          (ensime-goto-source-location pos where))
      (not (message "Sorry, ENSIME couldn't find the definition.")))))

(defun ensime-files-equal-p (f1 f2)
  "Return t if file-names refer to same file."
  (equal (file-truename (expand-file-name f1))
         (file-truename (expand-file-name f2))))


(defun ensime-window-showing-file (file)
  (catch 'result
    (dolist (w (window-list))
      (let* ((buf (window-buffer w))
	     (window-file (buffer-file-name buf)))
	(when (and window-file
		   (ensime-files-equal-p file window-file))
	  (throw 'result w))))))

(defun ensime-window-showing-buffer (buffer)
  (catch 'result
    (dolist (w (window-list))
      (let* ((buf (window-buffer w)))
	(when (equal buf buffer)
	  (throw 'result w))))))

(defun ensime-point-at-bol (file line)
  (with-current-buffer (find-buffer-visiting file)
    (save-excursion
      (ensime-goto-line line)
      (point))))

(defun ensime-goto-source-location (pos &optional where)
  "Move to the source location POS. Don't open
 a new window or buffer if file is open and visible already."
  (let* ((file (ensime-pos-effective-file pos))
	 (file-visible-window (ensime-window-showing-file file)))

    (when (not file-visible-window)
      (ensime-find-file-from-pos pos (eq where 'window) (eq where 'frame))
      (setq file-visible-window
	    (ensime-window-showing-file file)))

    (with-current-buffer (window-buffer file-visible-window)
      (let ((pt (cond
                 ((integerp (ensime-pos-offset pos))
                  (ensime-internalize-offset (ensime-pos-offset pos)))
                 ((integerp (ensime-pos-line pos))
                  (ensime-point-at-bol file (ensime-pos-line pos)))
                 (t 0))))
	(goto-char pt)
        (set-window-point file-visible-window pt)))))

(defun ensime-find-file-from-pos (pos other-window-p other-frame-p)
  (let* ((archive (ensime-pos-archive pos))
         (entry (ensime-pos-file pos))
         (effective-file (ensime-pos-effective-file pos))
         (existing-buffer (get-file-buffer effective-file)))
    (when archive
      (if existing-buffer
          (block nil
            (if other-window-p
                (switch-to-buffer-other-window existing-buffer)
              (switch-to-buffer existing-buffer))
            (return))
        (with-temp-buffer
          (archive-zip-extract archive entry)
          (make-directory (file-name-directory effective-file) t)
          (let ((backup-inhibited t))
            (write-file effective-file)))))

    (cond (other-window-p (find-file-other-window effective-file))
	  (other-frame-p (find-file-other-frame effective-file))
	  (t (find-file effective-file)))

    (let* ((config (ensime-config-for-buffer))
           (dep-src-dir (ensime-source-jars-dir config)))
      (when (ensime-path-includes-dir-p effective-file dep-src-dir)
        (with-current-buffer (get-file-buffer effective-file)
          (setq buffer-read-only t))))))

;; Compilation result interface

(defun ensime-show-compile-result-buffer (notes-in)
  "Show a popup listing the results of the last build."

  (ensime-with-popup-buffer
   (ensime-compile-result-buffer-name t t)
   (use-local-map ensime-compile-result-map)
   (ensime-insert-with-face
    "Latest Compilation Results (q to quit, g to refresh, TAB to jump to next error)"
    'font-lock-constant-face)
   (if (null notes-in)
       (insert "\n0 errors, 0 warnings.")
     (save-excursion

       ;; Group notes by their file and sort by
       ;; position in the buffer.
       (let ((notes-by-file (make-hash-table :test 'equal)))
	 (dolist (note notes-in)
	   (let* ((f (ensime-note-file note))
		  (existing (gethash f notes-by-file)))
	     (puthash f (cons note existing) notes-by-file)))
	 (maphash (lambda (file-heading notes-set)
		    (let ((notes (sort (copy-list notes-set)
				       (lambda (a b) (< (ensime-note-beg a)
							(ensime-note-beg b))))))

		      ;; Output file heading
		      (ensime-insert-with-face
		       (concat "\n" file-heading "\n")
		       'font-lock-comment-face)

		      ;; Output the notes
		      (dolist (note notes)
			(destructuring-bind
			    (&key severity msg beg
				  end line col file &allow-other-keys) note
			  (let ((face (case severity
					(error 'ensime-compile-errline)
					(warn 'ensime-compile-warnline)
					(info font-lock-string-face)
					(otherwise font-lock-comment-face)))
				(header (case severity
					  (error "ERROR")
					  (warn "WARNING")
					  (info "INFO")
					  (otherwise "MISC")))
				(p (point)))
			    (insert (format "%s: %s : line %s"
					    header msg line))
			    (ensime-make-code-link p (point) file beg face)))
			(insert "\n"))))
		  notes-by-file)))
     (forward-button 1))))


;; Compilation on request

(defun ensime-typecheck-current-buffer ()
  "Re-typecheck the current buffer and updates the last-typecheck time.."
  (interactive)
  (setf (ensime-last-typecheck-run-time (ensime-connection)) (float-time))
  (ensime-rpc-async-typecheck-buffer 'identity))

(defun ensime-save-and-typecheck-current-buffer ()
  "A compatibility shim. Writes the buffer and then invokes ensime-typecheck-current-buffer."
  (ensime-write-buffer nil t)
  (ensime-typecheck-current-buffer))

(defun ensime-reload-open-files ()
  "Make the ENSIME server forget about all files ; reload .class files
in the project's path ;  then reload only the Scala files that are
currently open in emacs."
  (interactive)
  (message "Unloading all files...")
  (ensime-rpc-unload-all)
  (message "Reloading open files...")
  (setf (ensime-last-typecheck-run-time (ensime-connection)) (float-time))
  (let* ((buffers (ensime-connection-visiting-buffers (ensime-connection)))
	 (files (-filter #'file-exists-p (-map #'buffer-file-name buffers))))
    (ensime-rpc-async-typecheck-files files 'identity)))

(defun ensime-typecheck-all ()
  "Send a request for re-typecheck of whole project to the ENSIME server.
   Current file is saved if it has unwritten modifications."
  (interactive)
  (message "Checking entire project...")
  (if (buffer-modified-p) (ensime-write-buffer nil t))
  (setf (ensime-awaiting-full-typecheck (ensime-connection)) t)
  (setf (ensime-last-typecheck-run-time (ensime-connection)) (float-time))
  (ensime-rpc-async-typecheck-all 'identity))

(defun ensime-show-all-errors-and-warnings ()
  "Show a summary of all compilation notes."
  (interactive)
  (let ((notes
         (append (ensime-java-compiler-notes (ensime-connection))
                 (ensime-scala-compiler-notes (ensime-connection)))))
    (ensime-show-compile-result-buffer notes)))

(defun ensime-sym-at-point (&optional point)
  "Return information about the symbol at point, using the an RPC request.
 If not looking at a symbol, return nil."
  (save-excursion
    (goto-char (or point (point)))
    (let* ((info (ensime-rpc-symbol-at-point))
           (pos (ensime-symbol-decl-pos info)))
      (if (null pos) (ensime-local-sym-at-point point)
        (let ((start (ensime-pos-offset pos))
              (name (plist-get info :local-name)))
          (setq start (ensime-internalize-offset start))
          (list :start start
                :end (+ start (string-width name))
                :name name))))))

(defun ensime-local-sym-at-point (&optional point)
  "Return information about the symbol at point. If not looking at a
 symbol, return nil."
  (save-excursion
    (goto-char (or point (point)))
    (let ((start nil)
          (end nil))
      (when (thing-at-point 'symbol)
        (save-excursion
          (search-backward-regexp "\\W" nil t)
          (setq start (+ (point) 1)))
        (save-excursion
          (search-forward-regexp "\\W" nil t)
          (setq end (- (point) 1)))
        (list :start start
              :end end
              :name (buffer-substring-no-properties start end))))))

(defun ensime-java-new-import (qualified-name)
  (format "import %s;\n" qualified-name))

(defun ensime-scala-new-import (qualified-name)
  (format "import %s\n" qualified-name))

(defun ensime-scala-new-import-grouped-package (base-package grouped-classes)
  (format "import %s.{ %s }" base-package grouped-classes))

(defun ensime-no-imports-in-buffer-p ()
  (looking-at "^\\s-*package\\s-"))

(defun ensime-import-block-in-buffer-p ()
  (looking-at "^\\s-*import\\s-"))

(defun ensime-same-base-package-p (current-import qualified-name)
  "Compare CURRENT-IMPORT's package to QUALIFIED-NAME's package, returning true if they are equal."
  (equal (->> current-import (s-split "\\.") -butlast)
         (->> qualified-name (s-split "\\.") -butlast)))

(defun ensime-past-starting-point (starting-point)
  "Past STARTING-POINT of excursion.
Should not insert past STARTING-POINT - move to beginning of line that STARTING-POINT is on.
STARTING-POINT is the point where the `ensime-insert-import' was invoked from."
  (when (>= (point) starting-point)
    (goto-char starting-point)
    (goto-char (point-at-bol))))

(defun ensime-indent-line ()
  (indent-region (point-at-bol) (point-at-eol)))

(defun ensime-insert-new-import-no-imports-in-buffer (starting-point java-scala-new-import qualified-name)
  "Insert new import when there are no current import statements in the buffer.
STARTING-POINT is the point where the `ensime-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (goto-char (point-at-eol))
    (newline)
    (newline)
    (ensime-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (ensime-indent-line)))

(defun ensime-insert-new-import-no-imports-or-package-in-buffer (starting-point java-scala-new-import qualified-name)
  "Insert new import when there are no current import statements or package statement in the buffer.
STARTING-POINT is the point where the `ensime-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (unless (looking-at "^\s*$")
     (newline)
     (backward-char 1))
    (ensime-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (ensime-indent-line)))

(defun ensime-insert-new-import-next-line (starting-point java-scala-new-import qualified-name)
  "Insert new import on the next line in the buffer.
STARTING-POINT is the point where the `ensime-insert-import' was invoked from.
JAVA-SCALA-NEW-IMPORT is a function to format the import statement for either java or scala.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (lambda ()
    (goto-char (point-at-eol))
    (if (equal (point) (point-max)) (newline) (forward-char 1))
    (ensime-past-starting-point starting-point)
    (save-excursion (insert (funcall java-scala-new-import qualified-name)))
    (ensime-indent-line)))

(defun ensime-insert-new-scala-import-grouped-package-next-line (current-import qualified-name)
  "Insert new grouped import on the next line in the buffer, overriding the whole line.
CURRENT-IMPORT is qualified name of the import line where the base package matches that of QUALIFIED-NAME.
QUALIFIED-NAME is the name to import.
Returns a function/closure to invoke the necessary buffer operations to perform the insertion."
  (let* ((current-import-components (->> current-import (s-split "\\.")))
         (base-package (->> current-import-components -butlast (s-join ".")))
         (current-imports (->> current-import-components -last-item))
         (qualified-class-name (->> qualified-name (s-split "\\.") -last-item))
         (new-imports (->> current-imports (s-chop-prefix "{") (s-chop-suffix "}")
                           (s-split ",") (-map 's-trim)
                           (cons qualified-class-name) (-sort 's-less?) (s-join ", "))))
    (lambda ()
      (if (equal (point) (point-max)) (newline) (forward-char 1))
      (kill-line)
      (->> (ensime-scala-new-import-grouped-package base-package new-imports)
           insert save-excursion)
      (ensime-indent-line))))

(defun ensime-move-to-end-of-multiline-import ()
  "Move point to the end of a multiline import."
  (when (not (string-match (rx (zero-or-more any) ?} (zero-or-more whitespace) eol) (thing-at-point 'line)))
      (goto-char (point-at-eol))
      (when (not (equal (point) (point-max)))
        (forward-line 1)
        (ensime-move-to-end-of-multiline-import)
      )))

(defun ensime-at-start-of-multiline-block ()
  "Returns t when at start of multiline block."
      (string-match (rx (zero-or-more anything) ?{ (zero-or-more (not (any ?})))) (thing-at-point 'line)))

(defun ensime-java-new-import-insertion-decisioning-in-import-block (insertion-range starting-point qualified-name)
  "Search through import statements in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (let ((looking-at-import? (looking-at "[\n\t ]*import\\s-\\(.+\\)\n"))
        (matching-import (match-string 1)))
    (cond
     ;; insert at the end of the import block
     ((not looking-at-import?) (ensime-insert-new-import-next-line starting-point 'ensime-java-new-import qualified-name))
     ;; insert next line
     ((not (s-less? matching-import qualified-name))
      (ensime-insert-new-import-next-line starting-point 'ensime-java-new-import qualified-name))
     ;; continue looking for insertion point
     (t
      (search-forward-regexp "^\\s-*import\\s-" insertion-range t)
      (goto-char (point-at-eol))
      (ensime-java-new-import-insertion-decisioning-in-import-block insertion-range starting-point qualified-name)))))

(defun ensime-scala-new-import-insertion-decisioning-in-import-block (insertion-range starting-point qualified-name)
  "Search through import statements in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (let ((looking-at-import? (looking-at "[\n\t ]*import\\s-\\(.+\\)\n"))
        (matching-import (match-string 1)))
    (cond
     ;; insert at the end of the import block
     ((not looking-at-import?)
      (when (ensime-at-start-of-multiline-block) (ensime-move-to-end-of-multiline-import))
      (ensime-insert-new-import-next-line starting-point 'ensime-scala-new-import qualified-name))
     ;; same base package, insert on next line, overriding the entire line
     ((ensime-same-base-package-p matching-import qualified-name)
      (ensime-insert-new-scala-import-grouped-package-next-line matching-import qualified-name))
     ;; insert next line
     ((not (s-less? matching-import qualified-name))
      (ensime-insert-new-import-next-line starting-point 'ensime-scala-new-import qualified-name))
     ;; continue looking for insertion point
     (t
      (search-forward-regexp "^\\s-*import\\s-" insertion-range t)
      (goto-char (point-at-eol))
      (ensime-scala-new-import-insertion-decisioning-in-import-block insertion-range starting-point qualified-name)))))

(defun ensime-insert-java-import (insertion-range starting-point qualified-name)
  "A simple java import insertion in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (cond
   ((ensime-no-imports-in-buffer-p)
    (ensime-insert-new-import-no-imports-in-buffer starting-point
                                                   'ensime-java-new-import
                                                   qualified-name))
   ((ensime-import-block-in-buffer-p)
    (unless (equal (point) (point-min)) (backward-char))
    (ensime-java-new-import-insertion-decisioning-in-import-block insertion-range
                                                     starting-point
                                                     qualified-name))
   ;; Neither import nor package: stay at beginning of buffer
   (t
    (ensime-insert-new-import-no-imports-or-package-in-buffer starting-point
                                                              'ensime-java-new-import
                                                              qualified-name))))

(defun ensime-insert-scala-import (insertion-range starting-point qualified-name)
  "A simple scala import insertion in buffer above INSERTION-RANGE and STARTING-POINT.
Decide what line to insert QUALIFIED-NAME."
  (cond
   ((ensime-no-imports-in-buffer-p)
    (ensime-insert-new-import-no-imports-in-buffer starting-point
                                                   'ensime-scala-new-import
                                                   qualified-name))
   ((ensime-import-block-in-buffer-p)
    (unless (equal (point) (point-min)) (backward-char))
    (ensime-scala-new-import-insertion-decisioning-in-import-block insertion-range
                                                                   starting-point
                                                                   qualified-name))
   ;; Neither import nor package: stay at beginning of buffer
   (t
    (ensime-insert-new-import-no-imports-or-package-in-buffer starting-point
                                                              'ensime-scala-new-import
                                                              qualified-name))))

(defun ensime-insert-import (qualified-name)
  "A simple ensime import insertion in buffer of QUALIFIED-NAME."
  (save-excursion
    (let ((insertion-range (point))
          (starting-point (point))
          (insert-import-fn (if (ensime-visiting-java-file-p) 'ensime-insert-java-import 'ensime-insert-scala-import)))
      (unless (search-backward-regexp "^\\s-*package\\s-" nil t)
        (goto-char (point-min)))
      (search-forward-regexp "^\\s-*import\\s-" insertion-range t)
      (goto-char (point-at-bol))
      (funcall (funcall insert-import-fn insertion-range starting-point qualified-name)))))

(defun ensime-ask-user-to-select-entry (title entries)
  "Prompts the user to select an entry of entries"
  (if ensime-use-helm
      (ensime-helm-select-entry entries title)
      (popup-menu* entries :point (point))
    ))

(defun ensime-import-type-at-point (&optional non-interactive)
  "Suggest possible imports of the qualified name at point.
 If user selects and import, add it to the import list."
  (interactive)
  (let* ((sym (ensime-local-sym-at-point))
	 (name (plist-get sym :name))
	 (name-start (plist-get sym :start))
	 (name-end (plist-get sym :end))
	 (suggestions (when name (ensime-rpc-import-suggestions-at-point (list name) 10))))
    (when (car-safe suggestions)
      (let* ((names (mapcar
		     (lambda (s)
		       (propertize (plist-get s :name)
				   'local-name
				   (plist-get s :local-name)))
		     (apply 'append suggestions)))
	     (selected-name
	      (if non-interactive (car names)
        (ensime-ask-user-to-select-entry "import-type" names)
    )))
	(when selected-name
	  (save-excursion
	    (when (and (not (equal selected-name name))
                       name-start name-end)
	      (goto-char name-start)
	      (delete-char (- name-end name-start))
	      (insert (ensime-short-local-name
                       (get-text-property
                        0 'local-name selected-name))))
	    (let ((qual-name
		   (ensime-strip-dollar-signs
		    (ensime-kill-txt-props selected-name))))
	      (ensime-insert-import qual-name)
	      (ensime-typecheck-current-buffer))))))))

;; Source Formatting

(defun ensime-format-source ()
  "Format the source in the current buffer using the Scalariform
 formatting library."
  (interactive)
  (let ((formatted (ensime-rpc-format-buffer)))
    (when formatted
      (when (eq 1 (coding-system-eol-type buffer-file-coding-system))
        (setq formatted (replace-regexp-in-string "\r$" "" formatted)))
      (let ((pt (point)))
        (erase-buffer)
        (insert formatted)
        (goto-char pt)))))

(defun ensime-revert-visited-files (files &optional typecheck)
  "files is a list of buffer-file-names to revert or lists of the form
 (visited-file-name disk-file-name) where buffer visiting visited-file-name
 will be reverted to the state of disk-file-name."
  (let ((pt (point)))
    (save-excursion
      (dolist (f files)
	(let* ((dest (cond ((stringp f) f)
			   ((listp f) (car f))))
	       (src (cond ((stringp f) f)
			  ((listp f) (cadr f)))))
	  (-when-let (buf (find-buffer-visiting dest))
            (with-current-buffer buf
              (insert-file-contents src nil nil nil t)
              ;; Rather than pass t to 'visit' the file by way of
              ;; insert-file-contents, we manually clear the
              ;; modification flags. This way the buffer-file-name
              ;; is untouched.
              (when (equal dest src)
                (clear-visited-file-modtime)
                (set-buffer-modified-p nil))
              (when typecheck
                (ensime-save-and-typecheck-current-buffer)))))))
    (goto-char pt)))

;; Expand selection

(defun ensime-set-selection-overlay (start end)
  "Set the current selection overlay, creating if needed."
  (ensime-clear-selection-overlay)
  (setq ensime-selection-overlay
	(ensime-make-overlay start end nil 'region nil)))

(defun ensime-clear-selection-overlay ()
  (when (and ensime-selection-overlay
	     (overlayp ensime-selection-overlay))
    (delete-overlay ensime-selection-overlay)))

(defun ensime-expand-selection-command ()
  "Expand selection to the next widest syntactic context."
  (interactive)
  (unwind-protect
      (let* ((continue t)
	     (ensime-selection-stack (list (list (point) (point))))
	     (expand-again-key 46)
	     (contract-key 44))
	(ensime-expand-selection (point) (point))
	(while continue
	  (message "(Type . to expand again. Type , to contract.)")
	  (let ((evt (read-event)))
	    (cond

	     ((equal expand-again-key evt)
	      (progn
		(clear-this-command-keys t)
		(ensime-expand-selection (mark) (point))
		(setq last-input-event nil)))

	     ((equal contract-key evt)
	      (progn
		(clear-this-command-keys t)
		(ensime-contract-selection)
		(setq last-input-event nil)))
	     (t
	      (setq continue nil)))))
	(when last-input-event
	  (clear-this-command-keys t)
	  (setq unread-command-events (list last-input-event))))

    (ensime-clear-selection-overlay)))

(defun ensime-set-selection (start end)
  "Helper to set selection state."
  (goto-char start)
  (command-execute 'set-mark-command)
  (goto-char end)
  (setq deactivate-mark nil)
  (ensime-set-selection-overlay start end))

(defun ensime-expand-selection (start end)
  "Expand selection to the next widest syntactic context."
  (ensime-with-buffer-written-to-tmp
   (file)
   (let* ((range (ensime-rpc-expand-selection
		  file start end))
	  (start (plist-get range :start))
	  (end (plist-get range :end)))
     (ensime-set-selection start end)
     (push (list start end) ensime-selection-stack))))

(defun ensime-contract-selection ()
  "Contract to previous syntactic context."
  (pop ensime-selection-stack)
  (let ((range (car ensime-selection-stack)))
    (when range
      (let ((start (car range))
	    (end (cadr range)))
	(ensime-set-selection start end)))))

(defun ensime-inspect-bytecode ()
  "Show the bytecode for the current method."
  (interactive)
  (let ((bc (ensime-rpc-method-bytecode buffer-file-name (ensime-current-line))))
    (if (not bc)
	(message "Could not find bytecode.")
      (progn
	(ensime-ui-show-nav-buffer "*ensime-method-bytecode-buffer*" bc t)))))

(defun ensime-ui-insert-method-bytecode (val)
  (destructuring-bind
      (&key class-name name bytecode &allow-other-keys) val
    (insert class-name)
    (insert "\n")
    (insert name)
    (insert "\n\n")
    (dolist (op bytecode)
      (ensime-insert-with-face (car op) 'font-lock-constant-face)
      (insert " ")
      (ensime-insert-with-face (cadr op) 'font-lock-variable-name-face)
      (insert "\n"))))

;; Uses UI

(defun ensime-show-uses-of-symbol-at-point ()
  "Display a hyperlinked list of the source locations
 where the symbol under point is referenced."
  (interactive)
  (let ((uses (ensime-rpc-uses-of-symbol-at-point)))
    (ensime-with-popup-buffer
     (ensime-uses-buffer-name t t)
     (use-local-map ensime-uses-buffer-map)


     (ensime-insert-with-face
      "TAB to advance to next use, q to quit"
      'font-lock-constant-face)
     (insert "\n\n\n")

     (dolist (pos uses)
       (let* ((file (ensime-pos-file pos))
              (pos-internal-offset (ensime-internalize-offset-for-file
                                    file
                                    (ensime-pos-offset pos)))

	      (range-start (- pos-internal-offset 80))
	      (range-end (+ pos-internal-offset 80))
	      (result (ensime-extract-file-chunk
		       file range-start range-end))
	      (chunk-text (plist-get result :text))
	      (chunk-start (plist-get result :chunk-start))
	      (chunk-start-line (plist-get result :chunk-start-line)))

	 (ensime-insert-with-face file 'font-lock-comment-face)
	 (ensime-insert-with-face
	  (format "\n------------------- @line %s -----------------------\n"
		  chunk-start-line)
	  'font-lock-comment-face)

	 (let ((p (point)))

	   ;; Insert the summary chunk
	   (insert chunk-text)

	   ;; Highlight the occurances
	   (let* ((external-from (plist-get pos :start))
                  (from (ensime-internalize-offset-for-file
                         file
                         (plist-get pos :start)))
		  (to (ensime-internalize-offset-for-file
                       file
                       (plist-get pos :end)))
		  (buffer-from (+ p (- from chunk-start)))
		  (buffer-to (+ p (- to chunk-start))))
	     (ensime-make-code-link
	      buffer-from buffer-to file external-from)))

	 (insert "\n\n\n")))
     (goto-char (point-min))
     (when uses (forward-button 1)))
    (ensime-event-sig :references-buffer-shown)))

(provide 'ensime-editor)

;; Local Variables:
;; End:
