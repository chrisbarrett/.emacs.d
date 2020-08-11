((magit-diff:magit-diff-mode "--ignore-space-change" "--no-ext-diff" "--stat")
 (magit-fetch "--prune")
 (magit-merge "--no-ff"))
