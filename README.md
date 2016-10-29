[![Melpa Status](http://melpa.org/packages/flycheck-ledger-badge.svg)](http://melpa.org/#/flycheck-ledger)
[![Melpa Stable Status](http://stable.melpa.org/packages/flycheck-ledger-badge.svg)](http://stable.melpa.org/#/flycheck-ledger)

flycheck-ledger
===============

This library provides a [flycheck][] checker for [Ledger] files.

Installation
------------

You'll need Emacs 24 for `flycheck`, so the recommended way to get
`flycheck-ledger` is as a package from the [MELPA][melpa]
repository. The version of `flycheck-ledger` there will always be
up-to-date.

If you insist on doing things the hard way, first ensure `flycheck` is
installed, then download this code and add the directory to your Emacs
`load-path`.

Then, in your `init.el`:

```lisp
(eval-after-load 'flycheck
  '(require 'flycheck-ledger))
```

Make sure that the `ledger` binary is present on Emacs' `exec-path`, or
customize `flycheck-ledger-executable` to point to the `ledger`
binary.

Usage
-----

When `flycheck` is enabled (e.g. with `global-flycheck-mode`), `ledger-mode`
buffers will be automatically checked using this checker.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See
[COPYING](https://github.com/purcell/flycheck-ledger/blob/master/COPYING)
for details.

About
-----

`flycheck-ledger` was written by [Steve Purcell](https://github.com/purcell).

<hr>

Author links:

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

[flycheck]: https://github.com/flycheck/flycheck
[tags]: https://github.com/purcell/flycheck-ledger/tags
[ledger]: https://ledger-cli.org/
[melpa]: http://melpa.org
