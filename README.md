# vi-tilde-fringe
[![MELPA](http://melpa.org/packages/vi-tilde-fringe-badge.svg)](http://melpa.org/#/vi-tilde-fringe)

Displays tildes in the fringe on empty lines a la Vi.

_Original idea from [/r/emacs][idea]_.

![screenshot](https://raw.githubusercontent.com/syl20bnr/vi-tilde-fringe/master/screenshot.png)

## Install

The package _will be available soon_ in [MELPA][].

If you have MELPA in `package-archives`, use

    M-x package-install RET vi-tilde-fringe-mode RET

If you don't, open `vi-tilde-fringe.el` in Emacs and call
`package-install-from-buffer`.

## Usage

To toggle the mode locally:

    M-x vi-tilde-fringe-mode

To toggle the mode globally:

    M-x global-vi-tilde-fringe-mode

To turn it on automatically only for programming modes:

    (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)

## Customization

Open the customization group buffer:

    M-x customize-group RET vi-tilde-fringe RET

There you can change the bitmap array or the face of the symbol drawn in the
fringe. By default the symbol is a tilde :-) and its face simply inherits from
`default.`

[MELPA]: http://melpa.org/
[idea]: https://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
