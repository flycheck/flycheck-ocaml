flycheck-ocaml — Flycheck for OCaml
===================================

[![License GPL 3][badge-license]][copying]

Add OCaml support to [Flycheck][]:

- Add a `ocaml-merlin` syntax checker using [Merlin][]

Installation
------------

As usual, from [MELPA][] or [MELPA Stable][].

In your [`Cask`][cask] file:

```cl
(source gnu)
(source melpa)

(depends-on "flycheck-ocaml")
```

In your `init.el`:

```cl
(with-eval-after-load 'flycheck
  (require 'flycheck-ocaml))

(add-hook 'tuareg-mode-hook #'merlin-mode)
```

Usage
-----

Just use Flycheck as usual in Tuareg Mode buffers.  Flycheck will automatically
use the new `ocaml-merlin` syntax checker if Merlin Mode is enabled.

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

See [`COPYING`][copying] for details.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/flycheck/flycheck-ocaml/blob/master/COPYING
[Flycheck]: http://www.flycheck.org
[Merlin]: https://github.com/the-lambda-church/merlin
[MELPA]: http://melpa.org
[MELPA Stable]: http://stable.melpa.org
[cask]: http://cask.readthedocs.org
