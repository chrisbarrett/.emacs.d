;;; js-hacks.el --- Hacks for js-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(unless (fboundp 'flatten-tree)
  (defun flatten-tree (tree)
    "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.
\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
    (let (elems)
      (while (consp tree)
        (let ((elem (pop tree)))
          (while (consp elem)
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (if elem (push elem elems))))
      (if tree (push tree elems))
      (nreverse elems))))

(provide 'js-hacks)

;;; js-hacks.el ends here
