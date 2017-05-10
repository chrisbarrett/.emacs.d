;;; blergh.el --- Rendering AST.  -*- lexical-binding: t; -*-
;;; Commentary:

;; Implements an interpreter for a simple layout DSL for magit sections.
;;
;; Copied out of blergh.el, until I decide whether to release a separate
;; package.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'subr-x)

;; Derived component support.

(defconst blergh--components (make-hash-table :test #'eq)
  "A mapping from the name of a component to its interpretation function.

When traversing a rendering AST, any list beginning with a symbol
is interpreted as a component reference.  That symbol is used to
look up an interpretation function in this table.  That function is
applied to any remaining elements of that cons.

The result of a function in this hash-table should be a new
rendering AST, or a string value to be inserted directly.")

(defmacro blergh-define-component (name arglist &rest body)
  "Define a rendering component.

NAME is the name of the component, which may thereafter be
referenced directly in rendering ASTs.

ARGLIST is the arguments that must be supplied to construct the
component.

BODY is the definition of the component."
          (declare (indent 2))
          (cl-assert (symbolp name))
          (cl-assert (listp arglist))
          (let ((fname (intern (format "blergh--generated--%s" name)))
                (docstring (format "Auto-generated component constructor function.

Creates instances of %s components, which may be referred to as
such in rendering ASTs." name)))
            `(progn
               (cl-defun ,fname ,arglist ,docstring ,@body)
               (puthash ',name #',fname blergh--components))))

(blergh-define-component line (inner-ast)
  `(,inner-ast
    (padding)))

(blergh-define-component key-value (width key value)
  (cl-assert (numberp width) t)
  (cl-assert (<= 0 width) t)
  (cl-assert (stringp key) t)
  (cl-assert (stringp value) t)
  (let* ((fmt-string (concat "%-" (number-to-string width) "s"))
         (str (concat (propertize (format fmt-string (concat key ": ")) 'face 'magit-header-line)
                      value)))
    (unless (string-blank-p (buffer-substring (line-beginning-position) (line-end-position)))
      (newline))
    `(line ,str)))


;; AST interpreter.

(defconst blergh--indentation-width 2)
(defconst blergh--space ?\ )

(defsubst blergh--indentation (indent-level)
  (make-string (* indent-level blergh--indentation-width) blergh--space))

(defsubst blergh--eval-string (s indent-level)
  (let ((value (if (string-empty-p (buffer-substring (line-beginning-position) (point)))
                   (concat (blergh--indentation indent-level) s)
                 s)))
    (insert value)))

(defsubst blergh--finalize-heading (start-pos)
  ;; This implementation is adapted from `magit-insert-heading'.

  ;; Apply heading face if no other face is set.
  (let ((heading (buffer-substring start-pos (line-end-position))))
    (unless (next-single-property-change 0 'face (concat "0" heading))
      (add-text-properties start-pos (point) '(face magit-section-heading))))
  (unless (bolp)
    (insert ?\n))

  ;; Update containing section to point to this heading.
  (setf (magit-section-content magit-insert-section--current) (point-marker)))

(defsubst blergh--finalize-list-item (start-pos)
  (save-excursion
    (goto-char start-pos)
    (goto-char (line-beginning-position))
    (skip-chars-forward " ")
    (unless (eq (char-after) ?-)
      (delete-char -2)
      (insert "- "))))

(defun blergh--append-sentinel (instructions sentinel)
  (append (list instructions) (list sentinel)))

(defun blergh-eval (ast &optional indent-level)
  "Evaluate AST as a set of instructions for inserting text into the current buffer."

  ;; The evaluator is implemented as a loop over an instruction stack. The
  ;; `instruction-stack' variable is a stack of AST instructions, the head of
  ;; which is the instruction to interpret. Its initial value is set to the
  ;; input to this function. After an instruction is interpreted, the item at
  ;; the top of the stack is popped. The loop ends when there are no more
  ;; instructions on the stack.
  ;;
  ;; If nested instructions are encountered in the AST, they are pushed onto the
  ;; stack, generally with a sentinel instruction to restore previous
  ;; interpreter state.

  (let ((instruction-stack (list ast))
        (indent-level (or indent-level 0)))

    (while instruction-stack
      (pcase (car instruction-stack)

        ;; Strings are inserted directly, possibly with indentation.

        ((and (pred stringp) s)
         (blergh--eval-string s indent-level)
         (!cdr instruction-stack))

        ;; Padding gets some special error checking to make sure it has no inner
        ;; AST, since I get `padding' and `indent' mixed up all the time.

        ((and `(padding . ,_rest) (guard _rest))
         (error "Padding takes no arguments"))
        (`(padding)
         (newline)
         (!cdr instruction-stack))

        ;; Indentation
        ;;
        ;; The current indentation level is tracked by the interpreter. When an
        ;; `indent' directive is encountered, the indent level is incremented
        ;; and the inner AST is pushed to the stack with a sentinel appended.
        ;; When the sentinel is encountered, the indentation level is decreased.

        (`(indent . ,inner-ast)
         (let ((next (blergh--append-sentinel inner-ast 'blergh--indent-sentinel)))
           (setq indent-level (1+ indent-level))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`blergh--indent-sentinel
         (setq indent-level (1- indent-level))
         (!cdr instruction-stack))

        ;; Properties
        ;;
        ;; To propertize some inserted text, the inner AST is pushed to the
        ;; stack with a sentinel appended. The sentinel records the properties
        ;; to apply and the start position of the span. Once the sentinel is
        ;; encountered, the end position of the span is known and properties can
        ;; be applied.

        (`(propertize ,spec . ,inner-ast)
         (let ((next (blergh--append-sentinel inner-ast `(blergh--propertize-sentinel ,(point) ,spec))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(blergh--propertize-sentinel ,start ,spec)
         (add-text-properties start (point) spec)
         (!cdr instruction-stack))

        ;; Bulleted lists
        ;;
        ;; A bulleted list is decomposed into a sequence of instructions, each
        ;; of which tracks its buffer positions using sentinel values.
        ;;
        ;; The bullet group is indented, and each item's start position is
        ;; recorded in a sentinel value. When an item's sentinel is encountered,
        ;; the item's dash is written to the buffer.

        (`(list . ,items)
         (let ((next `(indent ,@(--map `(blergh--list-item . ,it) items))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(blergh--list-item . ,inner-ast)
         (let ((next (blergh--append-sentinel inner-ast `(blergh--list-item-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(blergh--list-item-sentinel . ,start)
         (blergh--finalize-list-item start)
         (!cdr instruction-stack))

        ;; Headings
        ;;
        ;; Heading insertion requires interpretation of an inner AST to build
        ;; the heading text. A special sentinel is appended to the inner AST
        ;; that tells the interpreter to finalise the heading after interpreting
        ;; the inner value.

        (`(heading ,inner-ast)
         (unless magit-insert-section--current (error "Eval AST: Inserting a heading, but not in a section"))
         (let ((next (blergh--append-sentinel inner-ast `(blergh--heading-sentinel . ,(point)))))
           (!cdr instruction-stack)
           (!cons next instruction-stack)))

        (`(blergh--heading-sentinel . ,start-pos)
         (blergh--finalize-heading start-pos)
         (!cdr instruction-stack))

        ;; Sections
        ;;
        ;; KLUDGE: The section insertion logic in magit has complex state. It's
        ;; easier just to evaluate recursively than try to reproduce that logic
        ;; in the interpreter. This is safe so long as section nesting doesn't
        ;; approach `max-lisp-eval-depth'.

        (`(section (,sym ,hide) . ,inner)
         (!cdr instruction-stack)
         (eval `(magit-insert-section (,sym nil ,hide)
                  (blergh-eval ',inner ,indent-level))))

        ;; Custom components
        ;;
        ;; If the current instruction is a list and its head is a symbol, look
        ;; it up in the component definition table. If the lookup succeeds,
        ;; evaluate the component's constructor function to derive an AST, and
        ;; push that AST onto the stack.

        ((and `(,component . ,args)
              (guard component)
              (guard (symbolp component)))
         (!cdr instruction-stack)

         (if-let (constructor (gethash component blergh--components))
             (!cons (apply constructor args) instruction-stack)
           (error "Component not defined: %s" component)))

        ;; Lists of instructions
        ;;
        ;; If the list being scrutinised does not begin with a symbol, it is
        ;; assumed to be a sequence of instructions. The items are pushed to the
        ;; stack.

        ((and (pred listp) actions)
         (!cdr instruction-stack)
         (setq instruction-stack (append actions instruction-stack)))

        ;; Heck, you've done the interpreter a frighten.

        (other
         (message "Stack: %s" instruction-stack)
         (error "Unknown AST instruction: %s" other))))))


;; Mode

(defvar blergh-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "p")   #'magit-section-backward)
    (define-key keymap (kbd "n")   #'magit-section-forward)
    (define-key keymap (kbd "M-p") #'magit-section-backward-sibling)
    (define-key keymap (kbd "M-n") #'magit-section-forward-sibling)
    (define-key keymap (kbd "C-i") #'magit-section-toggle)
    (define-key keymap (kbd "^")   #'magit-section-up)
    (define-key keymap [tab]       #'magit-section-toggle)
    (define-key keymap [C-tab]     #'magit-section-cycle)
    (define-key keymap [M-tab]     #'magit-section-cycle-diffs)
    (define-key keymap [S-tab]     #'magit-section-cycle-global)
    (define-key keymap (kbd "q")   #'quit-window)
    (define-key keymap (kbd "h")   #'describe-mode)

    keymap)
  "Keymap for `blergh-mode'.")

(define-derived-mode blergh-mode special-mode "Blergh"
  "Base mode for Blergh buffers.

\\{blergh-mode-map}"
  :group 'blergh
  (read-only-mode)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))


;; Evil compatability

(autoload 'evil-define-key "evil-core")
(autoload 'evil-set-initial-state "evil-core")

(with-eval-after-load 'evil
  (evil-set-initial-state 'blergh-mode 'motion)

  (evil-define-key 'motion blergh-mode-map
    (kbd "p")   #'magit-section-backward
    (kbd "n")   #'magit-section-forward
    (kbd "M-p") #'magit-section-backward-sibling
    (kbd "M-n") #'magit-section-forward-sibling
    (kbd "C-i") #'magit-section-toggle
    (kbd "^")   #'magit-section-up
    [tab]       #'magit-section-toggle
    [C-tab]     #'magit-section-cycle
    [M-tab]     #'magit-section-cycle-diffs
    [S-tab]     #'magit-section-cycle-global

    [remap evil-next-line] #'next-line
    [remap evil-previous-line] #'previous-line
    [remap evil-next-visual-line] #'next-line
    [remap evil-previous-visual-line] #'previous-line

    (kbd "q") #'quit-window
    (kbd "h") #'describe-mode))


(provide 'blergh)

;;; blergh.el ends here
