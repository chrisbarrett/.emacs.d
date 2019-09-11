;;; org-hydras.el --- Hydras for orgmode  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'hydra)
(require 'org)
(require 'org-funcs)
(require 'pretty-hydra)

(defun org-hydras-open-babel-block-in-buffer ()
  "Eval the org src block at point and show the result in a new buffer."
  (interactive)
  (org-babel-execute-src-block nil nil '((:results . "output")
                                         (:format . "raw")
                                         (:exports . "results")))
  (org-babel-open-src-block-result)
  (org-babel-remove-result))

(defhydra org-babel ()
  "
[_i_] block info         [_?_] check header
[_x_] split/insert new   [_I_] insert header arg  [_q_] quit

Execute   ^^              Navigate        ^^^^               Tangle    ^^
──────────^^───────────── ────────────────^^^^────────────── ──────────^^─────────────
[_e_] block               [_n_] next   [_p_] previous        [_t_] tangle
[_b_] buffer              [_h_] head                      ^^ [_c_] clean
[_s_] subtree             [_u_] up heading                ^^ [_d_] detangle
[_k_] delete result                                     ^^^^ [_j_] jump to org file
[_o_] execute and open    [_B_] goto named block
          ^^              [_N_] goto named result
          ^^              [_r_] goto this block's result
Sessions
───────────^^^^^^──────────────────────────────────────────────────
[_z_] show session   [_Z_] show code and session   [_l_] load block
"
  ("?" org-babel-check-src-block :exit t)
  ("B" org-babel-goto-named-src-block)
  ("I" org-babel-insert-header-arg)
  ("N" org-babel-goto-named-result)
  ("b" org-babel-execute-buffer :exit t)
  ("c" org-babel-tangle-clean :exit t)
  ("d" org-babel-detangle)
  ("e" org-babel-execute-maybe)
  ("h" org-babel-goto-src-block-head)
  ("i" org-babel-view-src-block-info :exit t)
  ("j" org-babel-tangle-jump-to-org :exit t)
  ("l" org-babel-load-in-session)
  ("k" org-babel-remove-result)
  ("n" org-babel-next-src-block)
  ("o" org-hydras-open-babel-block-in-buffer :exit t)
  ("p" org-babel-previous-src-block)
  ("r" org-babel-open-src-block-result)
  ("s" org-babel-execute-subtree :exit t)
  ("t" org-babel-tangle :exit t)
  ("u" outline-up-heading)
  ("x" org-babel-demarcate-block :exit t)
  ("z" org-babel-switch-to-session :exit t)
  ("Z" org-babel-switch-to-session-with-code :exit t)
  ("q" hydra-keyboard-quit :exit t))

(pretty-hydra-define org-clock
  (:hint nil
   :color teal
   :title (hydra-title-with-octicon "clock" "Clocking"))
  ("Clocking"
   (("i" org-funcs-punch-in "punch in")
    ("o" org-funcs-punch-out "punch out")
    ("r" org-funcs-punching-resume "resume"))
   ""
   (("g" org-clock-goto "goto"))))

(provide 'org-hydras)

;;; org-hydras.el ends here
