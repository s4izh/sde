# 0001. Dotfiles as plain symlinks, not home-manager

- Status: Accepted
- Date: 2026-10-11

## Context

home-manager can own every config file, but it copies them into the Nix store:
each edit needs a rebuild, and the files only exist on machines running Nix.
The same dotfiles are also used on Ubuntu (guix home) and WSL.

## Decision

Keep program configs as plain files in `dotfiles/`, `nvim/` and `emacs/`, and
symlink them into `$HOME` with `make softlinks-create`. The link list lives in
`nixos/config.mk`.

home-manager is only used for programs whose config is easier to generate from
Nix (beets, ssh, waybar, ...). No `mkOutOfStoreSymlink` to work around the
store for files that could just be in the link list.

## Consequences

- Edits apply immediately, no rebuild.
- The same files work on any distro.
- Installing packages and linking configs are two separate steps: a fresh
  machine needs both `nixos-rebuild` and `make softlinks-create`.
- Per-machine differences can't use Nix conditionals; see
  [0002](0002-per-host-config-via-hostname-links.md).
