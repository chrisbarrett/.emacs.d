;;; stylus-mode.el --- Major mode for stylus files
;;
;; Copyright (c) 2007, 2008 Nathan Weizenbaum
;; Copyright (c) 2009-2013 Daniel Mendler
;; Copyright (c) 2012-2014 Bozhidar Batsov
;; Copyright (c) 2016 Henrik Lissner
;; Copyright (c) 2016 Vlad-Ștefan Harbuz
;;
;; Author: Vlad-Ștefan Harbuz
;; Inspired by: https://github.com/hlissner/pug-mode
;; URL: http://github.com/vladh
;; Version: 1.0
;; Keywords: stylus, css, language
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl))
(require 'js)

;; User definable variables

(defgroup stylus nil
  "Support for the Stylus template language."
  :group 'languages
  :prefix "stylus-")

(defcustom stylus-mode-hook nil
  "Hook run when entering Stylus mode."
  :type 'hook
  :group 'stylus)

(defcustom stylus-backspace-backdents-nesting t
  "Non-nil to have `stylus-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'stylus)

(defvar stylus-indent-function 'stylus-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")


;; Font lock

(defconst stylus-tags-re
  (concat "^ *\\("
          (regexp-opt
           '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
             "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body"
             "br" "button" "canvas" "caption" "center" "cite" "code" "col"
             "colgroup" "command" "datalist" "dd" "del" "details" "dialog" "dfn"
             "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figure" "font" "footer"
             "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6"
             "head" "header" "hgroup" "hr" "html" "i"
             "iframe" "img" "input" "ins" "keygen" "kbd" "label" "legend" "li" "link"
             "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object"
             "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp"
             "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span"
             "strike" "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea" "tfoot"
             "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "xmp") 'words)
          "\\)")
  "Regex of all html4/5 tags.")

(defconst stylus-selfclosing-tags-re
  (concat "^ *"
          (regexp-opt
           '("meta" "title" "img" "area" "base" "br" "col" "command" "embed" "hr" "input"
             "link" "param" "source" "track" "wbr") t)))

(defconst stylus-keywords-re
  (concat "^ *\\(?:- \\)?" (regexp-opt '("extends" "block") t)))

(defconst stylus-control-re
  (concat "^ *\\(- \\)?\\("
          (regexp-opt
           '("if" "unless" "while" "until" "else" "for"
             "begin" "elsif" "when" "default" "case" "var'"

             "extends" "block" "mixin"
             ) 'words)
          "\\)"))

;; Helper for nested block (comment, embedded, text)
(defun stylus-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(\\(?:\\1" (make-string tab-width ? ) ".*\\| *\\)\n\\)*"))

(defconst stylus-colours
  (eval-when-compile
    (regexp-opt
     '("black" "silver" "gray" "white" "maroon" "red"
       "purple" "fuchsia" "green" "lime" "olive" "yellow" "navy"
       "blue" "teal" "aqua")))
  "Stylus keywords.")

(defconst stylus-keywords
  (eval-when-compile
    (regexp-opt
     '("return" "if" "else" "unless" "for" "in" "true" "false")))
  "Stylus keywords.")

(defvar stylus-font-lock-keywords
  `(
    (,"^[ {2,}]+[a-z0-9_:\\-]+[ ]" 0 font-lock-variable-name-face)
    (,"\\(::?\\(root\\|nth-child\\|nth-last-child\\|nth-of-type\\|nth-last-of-type\\|first-child\\|last-child\\|first-of-type\\|last-of-type\\|only-child\\|only-of-type\\|empty\\|link\\|visited\\|active\\|hover\\|focus\\|target\\|lang\\|enabled\\|disabled\\|checked\\|not\\)\\)*" . font-lock-type-face) ;; pseudoSelectors
    (,(concat "[^_$]?\\<\\(" stylus-colours "\\)\\>[^_]?")
     0 font-lock-constant-face)
    (,(concat "[^_$]?\\<\\(" stylus-keywords "\\)\\>[^_]?")
     0 font-lock-keyword-face)
    (,"#\\w[a-zA-Z0-9\\-]+" 0 font-lock-keyword-face) ; id selectors (also colors...)
    (,"\\([.0-9]+:?\\(em\\|ex\\|px\\|mm\\|cm\\|in\\|pt\\|pc\\|deg\\|rad\\|grad\\|ms\\|s\\|Hz\\|kHz\\|rem\\|%\\)\\b\\)" 0 font-lock-constant-face)
    (,"\\b[0-9]+\\b" 0 font-lock-constant-face)
    (,"\\.\\w[a-zA-Z0-9\\-]+" 0 font-lock-type-face) ; class names
    (,"$\\w+" 0 font-lock-variable-name-face)
    (,"@\\w[a-zA-Z0-9\\-]+" 0 font-lock-preprocessor-face) ; directives and backreferences
    ))

(defconst stylus-embedded-re "^ *:[a-z0-9_-]+")
(defconst stylus-plain-re "^ *[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\.")
(defconst stylus-comment-re "^ *-?//-?")

(defun* stylus-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (unless (looking-at "\\.$")
        (beginning-of-line)
        (unless (or (looking-at stylus-embedded-re)
                    (looking-at stylus-comment-re))
          (return-from stylus-extend-region)))
      (setq font-lock-beg (point))
      (stylus-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defvar jade-tag-declaration-char-re "[-a-zA-Z0-9_.#+]"
  "Regexp used to match a character in a tag declaration")

(defun jade-goto-end-of-tag ()
  "Skip ahead over whitespace, tag characters (defined in
`jade-tag-declaration-char-re'), and paren blocks (using
`forward-sexp') to put point at the end of a full tag declaration (but
before its content). Use when point is inside or to the left of a tag
declaration"
  (interactive)

  ;; skip indentation characters
  (while (looking-at "[ \t]")
    (forward-char 1))

  (while (looking-at jade-tag-declaration-char-re)
    (forward-char 1))
  (if (looking-at "(")
      (forward-sexp 1)))

;; Mode setup

(defvar stylus-syntax-table
  (let ((syntable (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 124b" syntable)
    (modify-syntax-entry ?* ". 23" syntable)
    (modify-syntax-entry ?\n "> b" syntable)
    (modify-syntax-entry ?' "\"" syntable)
    syntable)
  "Syntax table for `stylus-mode'.")

(defvar stylus-mode-map (make-sparse-keymap))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'stylus-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode stylus-mode stylus-parent-mode "Stylus"
  "Major mode for editing Stylus files."
  (set-syntax-table stylus-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'stylus-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'stylus-indent-line)
  (set (make-local-variable 'indent-region-function) 'stylus-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'electric-indent-chars) nil)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '(stylus-font-lock-keywords))
  (use-local-map stylus-mode-map))

;; Useful functions

(defun stylus-comment-block ()
  "Comment the current block of Stylus code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "/")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (stylus-mark-sexp)
      (stylus-reindent-region-by tab-width))))

(defun stylus-uncomment-block ()
  "Uncomment the current block of Stylus code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at stylus-comment-re))
      (stylus-up-list)
      (beginning-of-line))
    (stylus-mark-sexp)
    (kill-line 1)
    (stylus-reindent-region-by (- tab-width))))

;; Navigation

(defun stylus-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun stylus-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun stylus-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Stylus is defined as a line of Stylus code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (stylus-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (stylus-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun stylus-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Stylus is defined as a line of Stylus code as well as any
lines nested beneath it."
  (interactive "p")
  (stylus-forward-sexp (if arg (- arg) -1)))

(defun stylus-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (stylus-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun stylus-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (stylus-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (stylus-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun stylus-mark-sexp ()
  "Marks the next Stylus block."
  (let ((forward-sexp-function 'stylus-forward-sexp))
    (mark-sexp)))

(defun stylus-mark-sexp-but-not-next-line ()
  "Marks the next Stylus block, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (stylus-mark-sexp)
  (let ((pos-of-end-of-line (save-excursion
                              (goto-char (mark))
                              (end-of-line)
                              (point))))
    (when (/= pos-of-end-of-line (mark))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line -1)
         (end-of-line)
         (point))))))

;; Indentation and electric keys

(defun stylus-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (or (looking-at-p stylus-comment-re)
      (looking-at-p stylus-embedded-re)
      (and (save-excursion
             (back-to-indentation)
             (not (memq (face-at-point) '(font-lock-preprocessor-face))))
           (not (looking-at-p stylus-selfclosing-tags-re))
           (loop for opener in `(,(concat "^ *\\([\\.#+]\\|" stylus-tags-re "\\)[^ \t]*\\((.+)\\)?\n")
                                 "^ *[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\.\n"
                                 "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
                                 ,stylus-control-re)
                 if (looking-at-p opener) return t
                 finally return nil))))

(defun stylus-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (stylus-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall stylus-indent-function)
             tab-width
           0)))))

(defun stylus-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`stylus-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) tab-width) tab-width)
             (stylus-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun stylus-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `tab-width' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (stylus-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) tab-width) tab-width))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun stylus-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation))
        (bound (mark)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) bound t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) bound nil)))))

(defun stylus-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `tab-width' spaces.

Set `stylus-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char-untabify arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if stylus-backspace-backdents-nesting
            (stylus-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (stylus-reindent-region-by (* (- arg) tab-width))
        (back-to-indentation)
        (pop-mark)))))

(defun stylus-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (stylus-mark-sexp-but-not-next-line)
  (kill-line 1)
  (stylus-reindent-region-by (* -1 tab-width)))

(defun stylus-indent-string ()
  "Return the indentation string for `tab-width'."
  (mapconcat 'identity (make-list tab-width " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))

;; Setup/Activation
(provide 'stylus-mode)
;;; stylus-mode.el ends here
