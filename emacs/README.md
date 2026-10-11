# Emacs

`simple/` is the config: a from-scratch setup, adding things step by step and
favoring vanilla Emacs over packages where reasonable.

The previous configs (`minimal/`, built on minimal-emacs.d, and `old/`) are in
[`.archive/emacs/`](../.archive/README.md).

## Deployment

```sh
make deploy   # links simple/ to ~/.emacs.d
```

## Third-party code

`simple/lisp/spaceway/` bundles the [spaceway theme](http://github.com/marktran/color-theme-spaceway)
by Mark Tran (BSD 3-Clause, see `spaceway/LICENSE`).
`spaceway-light-theme.el` is not third-party: it's a light-mode variant written
for this repo.
