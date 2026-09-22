# Guix based config

## `guix build`

Build a specific package of the repo:

```
guix build -L modules '(@ (sergio packages name) package-name))
```

Example:

```
guix build -L modules '(@ (sergio packages neovim) neovim-latest))
```

## Locking channels

See the `ubuntu/guix` subdir of this repo to check how it's done.
