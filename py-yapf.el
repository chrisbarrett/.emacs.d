;;; py-yapf.el --- Use yapf to beautify a Python buffer

;; Copyright (C) 2015-2016, Friedrich Paetzke <f.paetzke@gmail.com>

;; Author: Friedrich Paetzke <f.paetzke@gmail.com>
;; URL: https://github.com/paetzke/py-yapf.el
;; Version: 2016.1

;;; Commentary:

;; Provides commands, which use the external "yapf"
;; tool to tidy up the current buffer according to Python's PEP8.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;;; Code:


(defgroup py-yapf nil
  "Use yapf to beautify a Python buffer."
  :group 'convenience
  :prefix "py-yapf-")


(defcustom py-yapf-options nil
  "Options used for yapf.

Note that `--in-place' is used by default."
  :group 'py-yapf
  :type '(repeat (string :tag "option")))


(defun py-yapf--call-executable (errbuf file)
  (apply 'call-process "yapf" nil errbuf nil
         (append py-yapf-options `("--in-place", file))))


(defun py-yapf--call ()
  (py-yapf-bf--apply-executable-to-buffer "yapf" 'py-yapf--call-executable nil "py" t))


;;;###autoload
(defun py-yapf-buffer ()
  "Uses the \"yapf\" tool to reformat the current buffer."
  (interactive)
  (py-yapf--call))


;;;###autoload
(defun py-yapf-enable-on-save ()
  "Pre-save hooked to be used before running py-yapf."
  (interactive)
  (add-hook 'before-save-hook 'py-yapf-buffer nil t))


;; BEGIN GENERATED -----------------
;; !!! This file is generated !!!
;; buftra.el
;; Copyright (C) 2015-2016, Friedrich Paetzke <f.paetzke@gmail.com>
;; Author: Friedrich Paetzke <f.paetzke@gmail.com>
;; URL: https://github.com/paetzke/buftra.el
;; Version: 0.6

;; This code is initially copied from go-mode.el (copyright the go-mode authors).
;; See LICENSE or https://raw.githubusercontent.com/dominikh/go-mode.el/master/LICENSE


(defun py-yapf-bf--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in py-yapf-bf--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)
                (pop kill-ring)))
             (t
              (error "invalid rcs patch or internal error in py-yapf-bf--apply-rcs-patch")))))))))


(defun py-yapf-bf--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun py-yapf-bf--apply-executable-to-buffer (executable-name
                                           executable-call
                                           only-on-region
                                           file-extension
                                           ignore-return-code)
  "Formats the current buffer according to the executable"
  (when (not (executable-find executable-name))
    (error (format "%s command not found." executable-name)))
  ;; Make sure tempfile is an absolute path in the current directory so that
  ;; YAPF can use its standard mechanisms to find the project's .style.yapf
  (let ((tmpfile (make-temp-file (concat default-directory executable-name)
                                 nil (concat "." file-extension)))
        (patchbuf (get-buffer-create (format "*%s patch*" executable-name)))
        (errbuf (get-buffer-create (format "*%s Errors*" executable-name)))
        (coding-system-for-read buffer-file-coding-system)
        (coding-system-for-write buffer-file-coding-system))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (or (funcall executable-call errbuf tmpfile)
            (ignore-return-code))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil
                                        patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (pop kill-ring)
              (message (format "Buffer is already %sed" executable-name)))

          (if only-on-region
              (py-yapf-bf--replace-region tmpfile)
            (py-yapf-bf--apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (pop kill-ring)
          (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
                     executable-name executable-name)))
    (kill-buffer patchbuf)
    (pop kill-ring)
    (delete-file tmpfile)))


;; py-yapf-bf.el ends here
;; END GENERATED -------------------


(provide 'py-yapf)


;;; py-yapf.el ends here
