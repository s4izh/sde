# Emacs

Three parallel, independent configs:

- `minimal/` - built on the [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) framework (git submodule), with `lisp/` and `post-init.el` as the actual config.
- `simple/` - from-scratch rewrite, adding things step by step, favoring vanilla Emacs over packages where reasonable.
- `old/` - previous config, kept around as reference. Not maintained.

Only one can be deployed as `~/.emacs.d` at a time.

## Deployment

```sh
make deploy-minimal
make deploy-simple
make deploy-old
```

`make deploy` is an alias for `make deploy-minimal`.

## Third-party code

Some configs include third-party Emacs Lisp directly instead of pulling it from a package archive:

- `minimal/lisp/spaceway/` and `simple/lisp/spaceway/` bundle the [spaceway theme](http://github.com/marktran/color-theme-spaceway) by Mark Tran (BSD 3-Clause, see `spaceway/LICENSE`). `old/lisp/spaceway/` has an earlier copy that's slightly out of sync since that config isn't maintained.
- `simple/lisp/spaceway/spaceway-light-theme.el` is not third-party - it's a light-mode variant of spaceway written for this repo.
