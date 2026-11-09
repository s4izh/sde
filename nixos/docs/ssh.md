# SSH keys and remote mounts

How my machines trust each other over SSH, and how another machine's home is
mounted at `~/mnt/<name>` on demand. The code lives in
[`keys/`](../keys), [`modules/sde/nixos/ssh-keys.nix`](../modules/sde/nixos/ssh-keys.nix)
and [`modules/sde/nixos/sshfs.nix`](../modules/sde/nixos/sshfs.nix); the per-host
choice in `hosts/<host>/default.nix`.

## Overview

```
 keys/
   users/<name>.pub    my user keys (~/.ssh/id_ed25519.pub of each machine)
   hosts/<host>.pub    host keys   (/etc/ssh/ssh_host_ed25519_key.pub)
        │
        ▼
 sde.sshKeys  (every host with sde.base, off on jsc)
   ├─ users/*  ──▶ sergio's authorized_keys      "let these keys log in as me"
   └─ hosts/*  ──▶ /etc/ssh/ssh_known_hosts      "this is what rx really is"
        │
        ▼
 sde.sshfs.mounts.rx.host = "rx"   (hosts/dell)
   fstab entry ──▶ home-sergio-mnt-rx.automount   watches ~/mnt/rx
                     │ first access
                     ▼
                   home-sergio-mnt-rx.mount       runs sshfs sergio@rx:/home/sergio
                     │ 10 min idle
                     ▼
                   unmounted again
```

## Keys

Every SSH connection checks identity in both directions:

| Key | Belongs to | Proves | Ends up in |
|---|---|---|---|
| `keys/users/dell.pub` | me, on dell | "I am sergio" to the server | `authorized_keys` on every host |
| `keys/hosts/rx.pub` | the machine rx | "I am the real rx" to the client | `/etc/ssh/ssh_known_hosts` on every other host |

Host keys normally get added by answering "yes" to *"The authenticity of host
… can't be established"*. Declaring them here means no machine ever asks, and
root (which runs the sshfs mount, with no terminal to answer) trusts them too.

Everything in `keys/` is a public key and safe to publish. Never put a private
key (`id_ed25519`, `ssh_host_*_key` without `.pub`) in the repo. The last field
of a key (`sergio@dell`) is a free-form comment; SSH ignores it.

### Adding a machine

1. On the new machine: `ssh-keygen -t ed25519` if `~/.ssh/id_ed25519` does not
   exist (no passphrase if it will be used by an sshfs mount, see below).
2. Copy `~/.ssh/id_ed25519.pub` to `keys/users/<host>.pub` and
   `/etc/ssh/ssh_host_ed25519_key.pub` to `keys/hosts/<host>.pub`.
   From another machine: `ssh-keyscan -t ed25519 <host>` prints the host key
   (check it against the file on the machine itself).
3. Commit, push, and rebuild **every machine that should accept it**. A key
   only works on hosts that have been rebuilt after adding it.

To keep a key off one host: `sde.sshKeys.exclude = [ "proton" ];`.

## Mounts

```nix
sde.sshfs.mounts.rx = {
  host = "rx";              # Tailscale MagicDNS name
  # remoteUser = "sergio";  # defaults
  # path = "/home/sergio";
  # idleTimeout = "10min";
};
```

- **On demand.** Nothing connects at boot. The first access to `~/mnt/rx`
  (`ls`, `cd`, opening a file) starts sshfs; after 10 min unused it unmounts.
- **Runs as root, acts as me.** systemd mounts it, but sshfs logs in as
  `sergio@rx` with my key (`IdentityFile`), so on rx it can do exactly what I
  can. Locally `uid/gid/idmap=user` shows the files as mine, and
  `default_permissions` makes the kernel enforce normal permissions (without
  it, `allow_other` would let any local user act as me on rx).
- **Fails fast.** If rx is unreachable, the attempt gives up after ~10 s
  (`ConnectTimeout`, `x-systemd.mount-timeout=15s`) and the access returns an
  error. The start limit is disabled, so the next access just tries again.
- **Survives drops.** `reconnect` + `ServerAlive*` reconnect after a network
  blip. While the link is down, processes touching the mount block for up to
  ~45 s before getting errors.
- The key has no passphrase because root cannot use my ssh-agent.

Good for: opening files from rx in local apps (browser, PDF viewer, image
viewer). Not good for: git, builds, grep or LSP over a big tree — do those over
`ssh rx` + tmux. inotify does not work over sshfs, so viewers will not
auto-reload on changes made on rx.

## Debugging

Work from the outside in: network, SSH as me, SSH as root, then systemd.

```bash
# 1. Is rx reachable?
tailscale status | grep rx
tailscale ping rx

# 2. Does SSH as me work without a password?
ssh -o BatchMode=yes rx whoami          # expects: sergio

# 3. Does it work the way the mount does it (as root, my key, system known_hosts)?
sudo ssh -o BatchMode=yes -i ~/.ssh/id_ed25519 sergio@rx whoami

# 4. What does systemd say?
systemctl status home-sergio-mnt-rx.automount home-sergio-mnt-rx.mount
journalctl -u home-sergio-mnt-rx.mount -b -n 50
findmnt ~/mnt/rx                        # mounted right now?

# 5. Mount by hand with verbose output, bypassing systemd
sudo systemctl stop home-sergio-mnt-rx.automount
mkdir -p /tmp/rx && sshfs -o debug,sshfs_debug,LogLevel=DEBUG3 rx: /tmp/rx   # stays in foreground
# ... Ctrl-C, then:
sudo systemctl start home-sergio-mnt-rx.automount
```

The unit name is the path with `/` turned into `-`:
`systemd-escape -p ~/mnt/rx` prints it.

### Common symptoms

| Symptom | Likely cause | Fix |
|---|---|---|
| `Permission denied (publickey)` in step 2/3 | rx was not rebuilt with this key, or the key file changed | add/check `keys/users/<host>.pub`, rebuild rx |
| `Host key verification failed` in step 3 | rx's host key is not in `keys/hosts`, or rx was reinstalled | update `keys/hosts/rx.pub` (`ssh-keyscan -t ed25519 rx`), rebuild this host |
| `WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED` | same as above; for my user also an old line in `~/.ssh/known_hosts` | `ssh-keygen -R rx`, then same fix |
| `Connection reset by peer` / `read: Connection reset` in the journal | SSH rejected the login; see steps 2–3 | |
| `Transport endpoint is not connected` | sshfs died but the mount stayed | `sudo umount -l ~/mnt/rx` (next access remounts) |
| `ls ~/mnt/rx` hangs | link is down and sshfs is waiting to reconnect | wait ~45 s, or `sudo umount -l ~/mnt/rx` |
| `~/mnt/rx` is an empty directory | the automount is not active (e.g. stopped, or failed before the start-limit fix) | `sudo systemctl reset-failed home-sergio-mnt-rx.{mount,automount}` and `sudo systemctl restart home-sergio-mnt-rx.automount` |
| `Start request repeated too quickly` | an old generation without the start-limit drop-in | rebuild, then the `reset-failed` above |
| Files show as owned by `nobody`/root | `uid`/`gid` wrong for this host | set `sde.sshfs.uid`/`gid` (check with `id`) |
| A PDF/HTML viewer does not reload | no inotify over sshfs | reload by hand, or work on rx and serve over `http://rx:<port>` |
