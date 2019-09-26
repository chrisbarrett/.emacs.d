;;; org-gcal-hacks.el --- Hacks for org-gcal.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)
(el-patch-feature org-gcal)



(with-eval-after-load 'org-gcal
  (with-no-warnings
    (el-patch-defun org-gcal-sync (&optional a-token skip-export silent)
      "Import events from calendars.
Using A-TOKEN and export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications."
      (interactive)
      (org-gcal--ensure-token)
      (when org-gcal-auto-archive
        (dolist (i org-gcal-fetch-file-alist)
          (with-current-buffer
              (find-file-noselect (cdr i))
            (org-gcal--archive-old-event))))
      (deferred:loop org-gcal-fetch-file-alist
        (lambda (x)
          (let* ((calendar-id (car x))
                 (calendar-file (cdr x))
                 (a-token (if a-token
                              a-token
                            (org-gcal--get-access-token)))

                 (skip-export skip-export)
                 (silent silent))
            (deferred:$
              (request-deferred
               (format org-gcal-events-url calendar-id)
               :type "GET"
               :params `(("access_token" . ,a-token)
                         ("key" . ,org-gcal-client-secret)
                         ("singleEvents" . "True")
                         ("orderBy" . "startTime")
                         ("timeMin" . ,(org-gcal--subtract-time))
                         ("timeMax" . ,(org-gcal--add-time))
                         ("grant_type" . "authorization_code"))
               :parser 'org-gcal--json-read)
              (deferred:nextc it
                (lambda (response)
                  (let
                      ((data (request-response-data response))
                       (status-code (request-response-status-code response))
                       (error-thrown (request-response-error-thrown response)))
                    (cond
                     ;; If there is no network connectivity, the response will
                     ;; not include a status code.
                     ((eq status-code nil)
                      (org-gcal--notify
                       "Got Error"
                       "Could not contact remote service. Please check your network connectivity.")
                      (error "Got error %S: %S" status-code error-thrown))
                     ((eq 401 (or (plist-get (plist-get (request-response-data response) :error) :code)
                                  status-code))
                      (org-gcal--notify
                       "Received HTTP 401"
                       "OAuth token expired. Now trying to refresh-token")
                      (deferred:$
                        (org-gcal--refresh-token)
                        (deferred:nextc it
                          (lambda (a-token)
                            (org-gcal-sync a-token skip-export silent)))))
                     ((eq 403 status-code)
                      (org-gcal--notify "Received HTTP 403"
                                        "Ensure you enabled the Calendar API through the Developers Console, then try again.")
                      (error "Got error %S: %S" status-code error-thrown))
                     ;; We got some 2xx response, but for some reason no
                     ;; message body.
                     ((and (> 299 status-code) (eq data nil))
                      (org-gcal--notify
                       (concat "Received HTTP" (number-to-string status-code))
                       "Error occured, but no message body.")
                      (error "Got error %S: %S" status-code error-thrown))
                     ((not (eq error-thrown nil))
                      ;; Generic error-handler meant to provide useful
                      ;; information about failure cases not otherwise
                      ;; explicitly specified.
                      (org-gcal--notify
                       (concat "Status code: " (number-to-string status-code))
                       (pp-to-string error-thrown))
                      (error "Got error %S: %S" status-code error-thrown))
                     ;; Fetch was successful. Return the list of events retrieved for
                     ;; further processing.
                     (t
                      (org-gcal--filter (plist-get data :items)))))))
              ;; Iterate over all events. For previously unretrieved events, add
              ;; them to the bottom of the file. For retrieved events, just collect
              ;; them into a list and pass to the next step.
              (deferred:nextc it
                (lambda (events)
                  (with-current-buffer (find-file-noselect calendar-file)
                    (goto-char (point-max))
                    (cl-loop
                     for event across events
                     if
                     (let* ((entry-id (org-gcal--format-entry-id
                                       calendar-id (plist-get event :id)))
                            (marker (org-id-find entry-id 'markerp)))
                       (cond
                        (marker
                         (org-gcal--event-entry-create
                          :entry-id entry-id
                          :marker marker
                          :event event))
                        (t
                         ;; Otherwise, insert a new entry into the
                         ;; default fetch file.
                         (insert "\n* ")
                         (org-gcal--update-entry calendar-id event)
                         nil)))
                     collect it))))
              ;; Find already retrieved entries and update them. This will update
              ;; events that have been moved from the default fetch file.
              (deferred:nextc it
                (lambda (entries)
                  (deferred:loop entries
                    (lambda (entry)
                      (deferred:$
                        (let ((marker (or (org-gcal--event-entry-marker entry)
                                          (org-id-find (org-gcal--event-entry-entry-id entry))))
                              (event (org-gcal--event-entry-event entry)))
                          (org-with-point-at marker
                            ;; If skipping exports, just overwrite current entry's
                            ;; calendar data with what's been retrieved from the
                            ;; server. Otherwise, sync the entry at the current
                            ;; point.
                            (set-marker marker nil)
                            (if (and skip-export event)
                                (progn
                                  (org-gcal--update-entry calendar-id event)
                                  (deferred:succeed nil))
                              (org-gcal-post-at-point nil skip-export))))
                        ;; Log but otherwise ignore errors.
                        (deferred:error it
                          (lambda (err)
                            (message "org-gcal-sync: error: %s" err))))))))
              (deferred:nextc it
                (lambda (_)
                  (el-patch-add
                    (with-current-buffer (find-file-noselect calendar-file)
                      (save-buffer)))
                  (unless silent
                    (org-gcal--notify "Completed event fetching ."
                                      (concat "Events fetched into\n" calendar-file)))
                  (deferred:succeed nil))))))))))

(provide 'org-gcal-hacks)

;;; org-gcal-hacks.el ends here
