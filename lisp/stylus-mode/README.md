# stylus-mode

`stylus-mode` offers Emacs support for [Stylus](http://stylus-lang.com/).
It is based on [pug-mode](https://github.com/hlissner/pug-mode) and
[jade-mode](https://github.com/brianc/jade-mode).

_Disclaimer:_ This package works, and I use it. However, I'm not the most
qualified person to write an emacs mode, so this is basically a hacked together
combination of `pug-mode` and `jade-mode`. Pull requests are welcome.

## Installation

`stylus-mode` isn't available on MELPA yet.

In the meantime, download `stylus-mode.el` and insert the following into your
emacs.d:

```elisp
(require 'stylus-mode)
```

For Spacemacs:

```elisp
dotspacemacs-additional-packages '(
  (stylus-mode :location (recipe :fetcher github :repo "vladh/stylus-mode"))
)
```

## Why not use jade-mode?

For pretty much the same reason that `pug-mode` was created to be used over
`jade-mode`, mainly `jade-mode`'s malfunctioning indentation and confusing
source.
