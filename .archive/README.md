# Archive

Things no longer in use, kept in case they're needed again. Every file keeps
its original path under `.archive/`, so restoring is a move back:

```sh
git mv .archive/dotfiles/.config/zsh dotfiles/.config/zsh
```

Nothing here is linked, loaded or built. If you restore something, check the
"Restoring" notes of its section: some items need a link in `nixos/config.mk`
or other pieces restored with them.

Archived on 2026-10-11.

## Configs

| Path | What | Why |
|------|------|-----|
| `dotfiles/.config/zsh/`, `dotfiles/.zprofile`, `dotfiles/.config/shell/profile` | zsh config | bash is the shell; zsh never loaded it |
| `dotfiles/.config/shell/profile_arch`, `profile_nix` | Old zsh/Arch-era login profiles | Unused, superseded by `shell/bash_profile` |
| `dotfiles/.config/ghostty/` | Ghostty terminal | Not used (kitty, foot, alacritty are) |
| `dotfiles/.config/ranger/` | ranger file manager | Not used |
| `dotfiles/.config/newsboat/` | newsboat RSS reader | Not used |
| `dotfiles/.config/kmonad/` | kmonad keyboard remapper | Not used |
| `dotfiles/.config/dunst/` | dunst notifications | mango uses swaync |
| `dotfiles/.config/mango/dms/`, `alacritty/dank-theme.toml`, `foot/dank-colors.ini` | DankMaterialShell generated configs | DMS not used |

Restoring: zsh needs `dotfiles/.config/zsh:.config/zsh` and
`dotfiles/.zprofile:.zprofile` back in `config.mk`; dunst needs
`dotfiles/.config/dunst:.config/dunst`. The others were never linked.
`.zshrc` sources `shell/shortcutrc`, `aliasrc` and `zshnameddirrc`, which no
longer exist.

## Emacs

| Path | What |
|------|------|
| `emacs/minimal/` | Config built on minimal-emacs.d: `post-init.el` + `lisp/` |
| `emacs/lisp/` | `org.el` and `ai.el`, loaded by `minimal/post-init.el` as `lisp/org.el`, `lisp/ai.el` |
| `emacs/old/` | Previous config (its compiled tree-sitter grammars were removed; nix provides them) |

Why: `simple/` is the only config in use. The plan is to bring the useful parts
of minimal-emacs.d into `simple/` without depending on the framework.

The minimal-emacs.d framework itself was a git submodule and is **not** in the
archive. To restore it:

```sh
git submodule add https://github.com/jamescherti/minimal-emacs.d.git emacs/minimal/minimal-emacs.d
git -C emacs/minimal/minimal-emacs.d checkout ce7818fcd31d6120ca68c6a7332a896ed1d8495b
```

Restoring: `minimal/` also needs `emacs/lisp/{org,ai}.el` moved into
`minimal/lisp/` (they were in the wrong place) and the `deploy-minimal`
target back in `emacs/Makefile` (see git history). Its `post-init.el` loads
`org-embed` from a path that doesn't exist; drop that block.
`nixos/modules/exwm.nix` still points to `emacs/old/exwm-start.sh`.

## Scripts (`dotfiles/.local/scripts/`)

### Duplicates (the kept version is in parentheses)

`cht.sh` (`cht`), `compilerluke` (`compiler`), `tmux/tmux-sessionizer2`
(`tmux-sessionizer`), `tmux/tmux-notes2` (`tmux-notes`, `pkm` session in
`~/work`).

### Menus

| Script | What |
|--------|------|
| `bemenu2` | bemenu wrapper with custom colors |
| `bemenu-run2` | bemenu-run launcher |
| `passmenu` | pass picker, Wayland and `--type` support |
| `passmenuwrapper` | Wrapper around passmenu |

### X11 / dwm

Archived even though the X11 WM configs are kept, so **sxhkd and dwm bindings
that call these are broken** until they're restored.

- `statusbar/`: dwmblocks modules (`sb-battery`, `sb-battery2`, `sb-clock`,
  `sb-cpu`, `sb-volume`, `sb-wrapper`).
- `dmenu*`: `dmenubrightness`, `dmenuconfig`, `dmenucounters`, `dmenudocs`,
  `dmenuhandler`, `dmenukblayout`, `dmenumonitor`, `dmenuopenuni`,
  `dmenuopgg`, `dmenupass`, `dmenuraco`, `dmenuremote`, `dmenuscreenshot`,
  `dmenusetfans`, `dmenusys`, `dmenutheme`, `dmenuunicode`.
- Screenshots: `maim-ss`, `maim-name-ss`, `maim-wiki`, `scrot-xclip`,
  `getss` (X11), `wayss`, `slurpshot` (old Wayland). mango uses `screenshot`.
- `autostart.sh`: X11 session autostart (setxkbmap, sxhkd).
- Called from sxhkd: `insert_n`, `launch_emacs`, `nixsearch`,
  `themeselector`. Called from river: `menu-system`.

### University and events

tmux sessions `tmux/tmux-{soa,soa-examen,zeos,hackupc,sdx,pti,rustygg,cf}`,
`tmux/res` (per-course resources), `lfuni`, `uni` (opens raco).

### System

| Script | What |
|--------|------|
| `mounter`, `unmount`, `mountgdrive` | Mount USB, Android, Google Drive (from voidrice) |
| `dvorak-layout`, `river-keyboard-selector` | Keyboard layout switchers |
| `laptop-monitor-off` | Turns off the laptop panel |
| `loginmanager` | Session picker |
| `hotreload` | Re-run a command on file change |
| `setwallpaper`, `setbg` | Wallpaper setters (`setbg` uses pywal) |
| `gpufan` | Manual GPU fan control via sysfs |
| `gamescope.sh` | gamescope launcher with VRR |
| `updateall` | System update helper |
| `ollama_server_wrapper` | Starts ollama by hand (now a NixOS service) |
| `fix-commits` | Rewrites commit authors with git-filter-repo |
| `vpn.sh` | VPN helper (`tmux/vpn_manager.sh` is the kept one) |

### Misc

| Script | What |
|--------|------|
| `bcn` | Bluetooth connect menu (Edesem/bluetooth-connect-script) |
| `ghs` | GitHub code search in Firefox |
| `ch` | Shows cheatsheets from `~/.local/share/cheat` |
| `repo`, `my-repo` | Note repo paths (`~/notes`, `~/pkm`) |
| `bookmark` | Saves the X11 selection to a bookmarks file |
| `linkhandler` | Opens URLs by type, used by newsboat |
| `larbs.sh` | Luke Smith's LARBS installer |
| `alacritty-pywall`, `alacritty-theme`, `ricer`, `waldl`, `print256colors` | Ricing tools |
| `audiomixer`, `autoclicker`, `ducksearch`, `notflix` | One-off utilities |
