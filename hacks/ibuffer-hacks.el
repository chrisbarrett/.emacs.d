;;; ibuffer-hacks.el --- Hacks for ibuffer.
;;; Commentary:
;;; Code:

(require 'el-patch)
(require 'f)

(el-patch-feature ibuffer)

(cl-eval-when (compile)
  (require 'ibuffer))

;; Show a horizontal rule using page-break-lines instead of using dashes.

(with-eval-after-load 'ibuffer
  (el-patch-defun ibuffer-update-title-and-summary (format)
    (ibuffer-assert-ibuffer-mode)
    ;; Don't do funky font-lock stuff here
    (let ((inhibit-modification-hooks t))
      (if (get-text-property (point-min) 'ibuffer-title)
          (delete-region (point-min)
                         (next-single-property-change
                          (point-min) 'ibuffer-title)))
      (goto-char (point-min))
      (add-text-properties
       (point)
       (progn
         (let ((opos (point)))
           ;; Insert the title names.
           (dolist (element format)
             (insert
              (if (stringp element)
                  element
                (pcase-let ((`(,sym ,min ,_max ,align) element))
                  ;; Ignore a negative min when we're inserting the title
                  (when (cl-minusp min)
                    (setq min (- min)))
                  (let* ((name (or (get sym 'ibuffer-column-name)
                                   (error "Unknown column %s in ibuffer-formats" sym)))
                         (len (length name))
                         (hmap (get sym 'header-mouse-map))
                         (strname (if (< len min)
                                      (ibuffer-format-column name
                                                             (- min len)
                                                             align)
                                    name)))
                    (when hmap
                      (setq
                       strname
                       (propertize strname 'mouse-face 'highlight 'keymap hmap)))
                    strname)))))
           (add-text-properties opos (point) '(ibuffer-title-header t))
           (insert "\n")
           ;; Add the underlines

           (el-patch-swap (let ((str (save-excursion
                                       (forward-line -1)
                                       (beginning-of-line)
                                       (buffer-substring (point) (line-end-position)))))
                            (apply #'insert (mapcar
                                             (lambda (c)
                                               (if (not (or (eq c ?\s)
                                                            (eq c ?\n)))
                                                   ?-
                                                 ?\s))
                                             str)))
                          (insert ""))
           (insert "\n"))
         (point))
       `(ibuffer-title t font-lock-face ,ibuffer-title-face))
      ;; Now, insert the summary columns.
      (goto-char (point-max))
      (if (get-text-property (1- (point-max)) 'ibuffer-summary)
          (delete-region (previous-single-property-change
                          (point-max) 'ibuffer-summary)
                         (point-max)))
      (if ibuffer-display-summary
          (add-text-properties
           (point)
           (progn
             (insert "\n")
             (dolist (element format)
               (insert
                (if (stringp element)
                    (make-string (length element) ?\s)
                  (pcase-let ((`(,sym ,min ,_max ,align) element))
                    ;; Ignore a negative min when we're inserting the title.
                    (when (cl-minusp min)
                      (setq min (- min)))
                    (let* ((summary
                            (if (get sym 'ibuffer-column-summarizer)
                                (funcall (get sym 'ibuffer-column-summarizer)
                                         (get sym 'ibuffer-column-summary))
                              (make-string
                               (length (get sym 'ibuffer-column-name))
                               ?\s)))
                           (len (length summary)))
                      (if (< len min)
                          (ibuffer-format-column summary
                                                 (- min len)
                                                 align)
                        summary))))))
             (point))
           '(ibuffer-summary t)))))

  )

;; Change the way groups are rendered.

(with-eval-after-load 'ibuffer
  (el-patch-defun ibuffer-insert-filter-group (name display-name filter-string format bmarklist)
    (add-text-properties
     (point)
     (progn
       (insert (el-patch-remove "[ ")
               display-name
               (el-patch-remove " ]"))
       (point))
     `(ibuffer-filter-group-name
       ,name
       font-lock-face ,ibuffer-filter-group-name-face
       keymap ,ibuffer-mode-filter-group-map
       mouse-face highlight
       help-echo ,(let ((echo '(if tooltip-mode
                                   "mouse-1: toggle marks in this group\nmouse-2: hide/show this filtering group"
                                 "mouse-1: toggle marks  mouse-2: hide/show")))
                    (if (> (length filter-string) 0)
                        `(concat ,filter-string
                                 (if tooltip-mode "\n" " ")
                                 ,echo)
                      echo))))
    (insert "\n")
    (when bmarklist
      (put-text-property
       (point)
       (progn
         (dolist (entry bmarklist)
           (ibuffer-insert-buffer-line (car entry) (cdr entry) format))
         (point))
       'ibuffer-filter-group
       name))
    (el-patch-add (insert "\n"))))

(provide 'ibuffer-hacks)

;;; ibuffer-hacks.el ends here
