;;; lsp-mode-hacks.el --- Hacks for lsp-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(cl-eval-when (compile)
  (require 'lsp-mode)
  (require 'lsp-ui-flycheck))

(el-patch-feature lsp-mode)

(with-eval-after-load 'lsp-mode
  (el-patch-defun lsp--on-diagnostics (workspace params)
    "Callback for textDocument/publishDiagnostics.
interface PublishDiagnosticsParams {
    uri: string;
    diagnostics: Diagnostic[];
}
PARAMS contains the diagnostics data.
WORKSPACE is the workspace that contains the diagnostics."
    (let* ((file (lsp--uri-to-path (gethash "uri" params)))
           (diagnostics (gethash "diagnostics" params))
           (buffer (lsp--buffer-for-file file))
           (workspace-diagnostics (lsp--workspace-diagnostics workspace)))
      (el-patch-wrap 2
        (when (hash-table-p workspace-diagnostics)
          (if (seq-empty-p diagnostics)
              (remhash file workspace-diagnostics)
            (when (or lsp-report-if-no-buffer buffer)
              (puthash file (seq-map #'lsp--make-diag diagnostics) workspace-diagnostics)))))

      (run-hooks 'lsp-diagnostics-updated-hook)

      (when buffer
        (save-mark-and-excursion
          (with-current-buffer buffer
            (run-hooks 'lsp-after-diagnostics-hook)))))))

(provide 'lsp-mode-hacks)

;;; lsp-mode-hacks.el ends here
