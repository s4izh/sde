# Guix home (Ubuntu, foreign distro)

`home-configuration.scm` is used with `guix home` on top of Ubuntu.

## Locked channels

`~/.config/guix/channels.scm` is a symlink to `channels-locked.scm` in this
directory, so every plain `guix pull` / `guix home reconfigure` rebuilds from
a pinned set of commits instead of silently tracking `master`.

- `channels-non-locked.scm`: the channel list with no `commit`/`branch`,
  used only to fetch the latest available revisions.
- `channels-locked.scm`: the same channels pinned to specific commits, as
  produced by `guix describe -f channels`. This is what `channels.scm`
  points to.

To update:

```
./update-channels.sh
```

This pulls from the non-locked list, re-locks by writing the new
`channels-locked.scm`, and pulls again so the active profile matches the
locked commits. Review the diff and commit `channels-locked.scm` to keep a
reproducible history of every update.

Setup (already done on this machine):

```
ln -sf $(pwd)/channels-locked.scm ~/.config/guix/channels.scm
```
