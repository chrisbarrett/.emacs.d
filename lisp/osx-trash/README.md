# osx-trash.el

[![License GPL 3](https://img.shields.io/github/license/lunaryorn/osx-trash.el.svg)][COPYING]
[![MELPA Stable](http://stable.melpa.org/packages/osx-trash-badge.svg)](http://stable.melpa.org/#/osx-trash)
[![MELPA](http://melpa.org/packages/osx-trash-badge.svg)](http://melpa.org/#/osx-trash)

Make `delete-by-moving-to-trash` do what you expect it to do on OS X.

For whatever reason GNU Emacs doesn’t support the system trash can on OS X.
Instead it’ll move files into the freedesktop.org trash can, which is, uhm, not
particularly useful behaviour on OS X.

This package makes Emacs do the right thing(TM) and move files to the real OS X
trash can.

**Note:** If you’re using the Emacs Mac Port, or if you’re not using OS X at
all, you’ll *not* need this package!

[badge-gpl3]: https://img.shields.io/badge/license-GPL_3-blue.svg
[COPYING]: https://github.com/lunaryorn/osx-trash.el/blob/master/COPYING

Setup
-----

Install this package from [MELPA][] or [MELPA Stable][], and add the following
to your init file:

```cl
(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)
```

Optionally, install [trash][] with `brew install trash`.  Otherwise this package
falls back to a little AppleScript helper, which works just as well, but is a
little slower.

[MELPA]: http://melpa.org
[MELPA Stable]: http://stable.melpa.org
[trash]: https://github.com/ali-rantakari/trash

License
=======

osx-trash is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

osx-trash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.
