# hcl-mode [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Major mode of [Hashicorp Configuration Language](https://github.com/hashicorp/hcl)


## Screenshot

![hcl-mode](image/hcl-mode.png)


## Installation

`hcl-mode` is available on [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org/)

You can install `hcl-mode` with the following command.

<kbd>M-x package-install [RET] hcl-mode [RET]</kbd>


## Features

- Syntax highlighting
- Indentation


## Customize Variables

#### `hcl-indent-level`(Default: `2`)

Indentation size


## Sample Configuration

```lisp
(custom-set-variables
 '(hcl-indent-level 4))
```

## See Also

- [terraform-mode](https://github.com/syohex/emacs-terraform-mode): A major mode of terraform configuration file


[travis-badge]: https://travis-ci.org/syohex/emacs-hcl-mode.svg
[travis-link]: https://travis-ci.org/syohex/emacs-hcl-mode
[melpa-link]: https://melpa.org/#/hcl-mode
[melpa-stable-link]: https://stable.melpa.org/#/hcl-mode
[melpa-badge]: https://melpa.org/packages/hcl-mode-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/hcl-mode-badge.svg
