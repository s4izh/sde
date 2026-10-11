# Architecture

How the pieces of this repo fit together. Decisions and their reasons live in
[`adr/`](adr/); this file describes the current state.

## Overview

```
                    ┌──────────────────────────┐
                    │  nixos/flake.nix         │  system packages, services,
                    │  (nixosConfigurations)   │  home-manager (programs only)
                    └────────────┬─────────────┘
                                 │ nixos-rebuild
                                 ▼
 dotfiles/, nvim/, emacs/ ──symlinks──▶ $HOME   (nixos/Makefile + config.mk)
                                 ▲
 guix/, ubuntu/, wsl/ ───────────┘   same dotfiles reused on non-NixOS systems
```

Two independent layers:

1. **System layer** (`nixos/`): what is installed and which services run. Needs
   a rebuild to change.
2. **Config layer** (`dotfiles/`, `nvim/`, `emacs/`): how programs behave. Plain
   files symlinked into `$HOME`, so edits apply immediately and work on any
   distro. See [ADR 0001](adr/0001-dotfiles-as-plain-symlinks.md).

## System layer: `nixos/`

| Path | Role |
|------|------|
| `flake.nix` | Inputs (nixpkgs unstable, home-manager, nixos-hardware) and one `nixosConfiguration` per host |
| `hosts/<host>/` | Per-machine entry point: imports modules, hardware config, host-only services |
| `modules/*.nix` | Feature modules imported directly by hosts (`desktop.nix`, `dev.nix`, `music.nix`, ...) |
| `modules/wm/` | One module per window manager (mango, river, ...) |
| `modules/sde/nixos/` | Option-based modules (`sde.desktop.enable`, `sde.virtualization.*`, `sde.latex.enable`) |
| `home/sergio/` | home-manager config: programs that need generated config (beets, ssh, waybar, ...) |
| `pkgs/` | Custom packages, exposed as `packages.<system>.*` |
| `parts/` | Experimental flake-parts version of the flake, not in use |

Every host gets a special arg `sde` with repo paths (`flakeRoot`,
`nixosPrefix`, `scripts`) so modules can read files from outside `nixos/`
(e.g. `dotfiles/etc/hosts`).

The flake is evaluated from the git tree: **new files must be `git add`ed**
before a rebuild can see them.

### Hosts

| Host | In flake | Notes |
|------|----------|-------|
| `rx` | yes | Main desktop, mango WM, ollama, navidrome |
| `zen` | yes | ASUS laptop |
| `jsc` | yes | KDE Plasma, deployed over SSH (`make deploy-jsc`) |
| `vm`, `z390`, `thinkcenter` | no | Kept for reference |

## Config layer

### Symlinks

`nixos/config.mk` holds `LINKS_FROM_SDE_TO_HOME`, a list of `src:dest` pairs.
`make softlinks-create` removes the previous links (tracked in
`~/.cache/sde/links`) and recreates them all.

### Per-host config

Some programs need slightly different config per machine. `config.mk` picks a
file by `hostname` and links it to a fixed path that the main config includes
optionally. See [ADR 0002](adr/0002-per-host-config-via-hostname-links.md).

Currently used by mango: `dotfiles/.config/mango/hosts/<hostname>.conf` (or
`default.conf`) → `~/.config/mango/host.conf`, loaded with `source-optional`.

### Editors

- `nvim/`: Neovim with the built-in `vim.pack` plugin manager. Plugins are
  declared in `lua/sergio/plugins.lua` and pinned in `nvim-pack-lock.json`;
  LSP server configs live in `lsp/`.
- `emacs/`: a single config, `simple/`, linked to `~/.emacs.d` with
  `make deploy`. See [`emacs/README.md`](../emacs/README.md).

## Archive

Unused configs and scripts live in `.archive/`, keeping their original paths.
[`.archive/README.md`](../.archive/README.md) lists what is there, why, and
what else must be restored with each item.

## Other systems

- `guix/`: Guix modules and packages.
- `ubuntu/guix/`: `guix home` on Ubuntu, reusing the same dotfiles.
- `wsl/`: own `deploy` script with a smaller link list.
