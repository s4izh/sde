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
