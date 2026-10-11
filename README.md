# sde

My development environment: system configs, dotfiles and editor setups for all my machines.

## Layout

```
sde/
├── nixos/      NixOS flake: hosts, modules, home-manager, custom packages
├── dotfiles/   Raw config files, symlinked into $HOME
├── nvim/       Neovim config (vim.pack, no plugin manager)
├── emacs/      Emacs configs (simple/, minimal/, old/)
├── guix/       Guix modules, packages and channels
├── ubuntu/     Guix home setup for Ubuntu (foreign distro)
└── wsl/        Bash setup and deploy script for WSL
```

## NixOS

The flake lives in `nixos/`. Active hosts:

| Host  | Machine                                              |
|-------|------------------------------------------------------|
| `rx`  | Main desktop: mango WM, ollama, navidrome            |
| `zen` | ASUS laptop                                          |
| `jsc` | KDE Plasma desktop, deployed remotely                |

`vm`, `z390` and `thinkcenter` are kept in `hosts/` but are not in the flake.

```sh
cd nixos
make help      # list all targets
make switch    # rebuild the current host
make update    # nix flake update
```

## Dotfiles

Dotfiles are not managed by home-manager, they are plain symlinks so edits
apply without a rebuild. They can be used outside NixOS.

The links are listed in `nixos/config.mk`:

```sh
cd nixos
make softlinks-create   # (re)create every link
make softlinks-remove   # remove them
```

Per-host files are linked based on `hostname`. For example, mango loads
`~/.config/mango/host.conf`, which points to
`dotfiles/.config/mango/hosts/<hostname>.conf`, or `hosts/default.conf` when the
host has no file of its own.

## Other systems

- **Guix**: see [`guix/README.md`](guix/README.md).
- **Ubuntu**: `guix home` on top of Ubuntu, see [`ubuntu/guix/README.md`](ubuntu/guix/README.md).
- **Emacs**: see [`emacs/README.md`](emacs/README.md).

## License

GPL-3.0, see [`LICENSE`](LICENSE).
