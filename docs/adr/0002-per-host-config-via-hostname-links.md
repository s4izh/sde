# 0002. Per-host config via hostname-selected links

- Status: Accepted
- Date: 2026-10-11

## Context

Some bindings only make sense on one machine. For example, on `rx` the volume
keys drive `monitor-toggle` and `gm`, while everywhere else they should change
the volume. Since dotfiles are plain symlinks ([0001](0001-dotfiles-as-plain-symlinks.md)),
there is no Nix layer to choose per host.

mango can't expand variables like the hostname in `source=` paths, and when two
binds use the same key it runs both, with no `unbind`. So the defaults can't
live in the main config and be overridden later.

## Decision

- Keep per-host files in the repo under `<config>/hosts/<hostname>.conf`, plus a
  `hosts/default.conf` fallback.
- `nixos/config.mk` reads `hostname` and adds one link from the matching file
  (or `default.conf`) to a fixed path, e.g. `~/.config/mango/host.conf`.
- The main config includes that path optionally (`source-optional=./host.conf`).
- The generated link is gitignored, since it lands inside the repo through the
  directory symlink.

## Consequences

- Host-specific settings are versioned and visible next to the main config.
- Settings that differ per host must live **only** in the host files, never in
  the main config, because a host file can't override the main config.
- A host with its own file gets nothing from `default.conf`.
- Changing hostname or adding a host file requires re-running
  `make softlinks-create`.
