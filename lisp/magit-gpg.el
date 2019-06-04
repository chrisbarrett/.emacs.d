;;; magit-gpg.el --- Show GPG commit info in magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 fice-t

;; Author: fice-t

;;; Commentary:

;; Show GPG commit info in magit.
;;
;; Sourced from:
;;
;;   https://gist.github.com/fice-t/c84c3bc7007d0d4bcacfeb2c0e42ac27

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)

(autoload 'magit-rev-insert-format "magit-git")

(defun magit-gpg--rev-format-items (format &optional rev args)
  (with-temp-buffer
    (magit-rev-insert-format format rev args)
    (split-string (buffer-string) "\0")))

(defun magit-gpg-insert-revision-gpg (&optional rev)
  (when-let* ((res (magit-gpg--rev-format-items "%G?%x00%GS%x00%GK%x00%GG" rev))
              (type (aref (car res) 0)))
    (unless (eq type ?N)
      (let* ((signer (cl-second res))
             (key (cl-third res))
             (raw (string-trim-right (cl-fourth res)))
             face
             status)
        (pcase type
          (?G (setq face 'magit-signature-good)
              (setq status "VALID"))
          (?B (setq face 'magit-signature-bad)
              (setq status "BAD"))
          (?U (setq face 'magit-signature-untrusted)
              (setq status "UNKNOWN"))
          (?X (setq face 'magit-signature-expired)
              (setq status "EXPIRED"))
          (?Y (setq face 'magit-signature-expired-key)
              (setq status "EXPIRED KEY"))
          (?R (setq face 'magit-signature-revoked)
              (setq status "REVOKED"))
          (?E (setq face 'magit-signature-error)
              (setq status "ERROR")))
        (magit-insert-section (gpg status (not (eq type ?E)))
          (insert "GPG Status: "
                  (propertize (or status "") 'face face)
                  " (press "
                  (substitute-command-keys "\\[magit-section-toggle]")
                  " to toggle raw output)\n")
          (unless (string= signer "")
            (magit-insert-section (gpg signer)
              (insert "GPG Signer: "
                      (propertize signer 'face face)
                      "\n")))
          (unless (string= key "")
            (magit-insert-section (gpg key)
              (insert "GPG Key:    "
                      (propertize key 'face face)
                      "\n")))
          (insert "\n")
          (magit-insert-heading)
          (magit-insert-section (gpg raw)
            (insert (propertize raw 'face face)
                    "\n\n")))))))

(provide 'magit-gpg)

;;; magit-gpg.el ends here
