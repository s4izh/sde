# Theming

How GTK, Qt, icons, cursor and the per-program colors are set, and how the
light/dark toggle reaches every running program. The code lives in
[`home/sergio/theming.nix`](../home/sergio/theming.nix); the per-host choice in
`hosts/<host>/default.nix`.

## Overview

```
 hosts/rx/default.nix
   sde.theming.theme = "manjaro"        which theme
   sde.theming.polarity = "dark"        mode after a switch / login
        │
        ▼
 home/sergio/theming.nix  (home-manager)
   ├─ gtk.*  qt.*  home.pointerCursor   theme of the current polarity
   ├─ dconf color-scheme                prefer-dark / prefer-light
   ├─ ~/.local/share/sde/polarity       "dark" | "light"   ◀── the contract
   ├─ ~/.local/share/kitty/sde-theme.conf
   └─ specialisation.{dark,light}       both modes prebuilt on every switch
        │
        ▼
 theme-mode toggle   (SUPER+SHIFT+t in mango)
   activates the other specialisation (no rebuild, no sudo)
        │
        ▼
 ~/.local/share/sde/polarity changes ──▶ every program reacts on its own
```

The rule: **one file says which mode is active, and every program reads that
file** (or dconf, which home-manager writes from the same option). Nothing
pushes a theme into a program; programs follow the file.

## Options

All under `sde.theming`, set from a host through
`home-manager.sharedModules` (see the commented block in
`hosts/rx/default.nix`).

| Option | Default | What |
|---|---|---|
| `enable` | `true` | Off under Plasma/GNOME, see [Desktop environments](#desktop-environments). |
| `theme` | `"adwaita"` | `adwaita` `breeze` `gruvbox` `nordic` `catppuccin` `tokyonight` `colloid` `manjaro` `stylix` |
| `polarity` | `"dark"` | Mode a switch or login starts in. `theme-mode` flips it at runtime. |
| `toggle` | `true` | Build both specialisations and install `theme-mode` + the watcher. |
| `stylixScheme` | `"gruvbox-dark-medium"` | base16 scheme for `stylix`, dark mode. |
| `stylixSchemeLight` | `"gruvbox-light-medium"` | Same, light mode. |
| `kitty.dark` | `null` | Kitty theme for dark mode, a file in `dotfiles/.config/kitty/themes` without `.conf`. `null` keeps `theme.conf`. |
| `kitty.light` | `"zenbones-light"` | Same, light mode. |

Listing and previewing the base16 schemes: see the comment next to
`stylixScheme` in `hosts/rx/default.nix`.

## Themes

Each entry of `themes` in `theming.nix` picks a GTK theme, icons, cursor and a
Qt setup for the current polarity (`pick dark light`).

| Theme | GTK dark / light | Icons | Qt |
|---|---|---|---|
| adwaita | adw-gtk3-dark / adw-gtk3 | Adwaita | adwaita(-dark) style |
| breeze | Breeze-Dark / Breeze | breeze(-dark) | Breeze + `kde` platform theme, colors in `kdeglobals` |
| gruvbox | Gruvbox-Dark / -Light | Gruvbox-Plus | Kvantum dark, GTK-following light |
| nordic | Nordic / Nordic-Polar | Nordzy | Kvantum dark, GTK-following light |
| catppuccin | mocha / latte, mauve | Papirus (catppuccin folders) | Kvantum |
| tokyonight | Tokyonight-Dark / -Light | Papirus | follows GTK |
| colloid | Colloid-Dark / -Light | Colloid | follows GTK |
| manjaro | Matcha-dark-sea / Matcha-light-sea | Papirus-Maia | follows GTK |
| stylix | generated from the base16 scheme | Papirus | Kvantum, generated |

Qt has three shapes, chosen per theme:

- **style**: a Qt style plugin (`adwaita`, `breeze`).
- **kvantum**: an SVG theme; `theming.nix` links it into
  `~/.config/Kvantum/` and writes `kvantum.kvconfig`.
- **platformTheme = "gtk3"**: Qt copies the GTK theme. Used when nixpkgs has no
  matching Kvantum theme (and for the light mode of gruvbox and nordic).

`stylix` is different: instead of picking packages, the Stylix home-manager
module generates GTK, Qt/Kvantum and the colors of every home-manager-managed
program (waybar included) from one base16 scheme. Emacs is excluded, it has its
own theme config.

## Light/dark toggle

### Specialisations

With `toggle = true` the home configuration gets two
[specialisations](https://nix-community.github.io/home-manager/options.xhtml#opt-specialisation),
`dark` and `light`, which only differ in `sde.theming.polarity`. A
`make switch` builds both, so changing mode is just running one of their
activation scripts: a few seconds, no rebuild, no sudo.

### `theme-mode`

```
theme-mode [toggle|dark|light|restore]
```

1. Reads the current mode from `~/.local/share/sde/polarity`.
2. Saves the requested mode in `~/.local/state/sde/theme-mode`.
3. Finds the generation of the last switch (the one with the specialisations)
   from the `ExecStart` of `home-manager-sergio.service`. After a toggle,
   `current-home` points to a specialisation, which has none of its own.
4. Runs `<generation>/specialisation/<mode>/activate`. Log in
   `~/.cache/sde/theme-mode.log`.
5. Signals kitty and waybar and sends a notification.

`restore` re-applies the saved mode. `make switch` and the boot-time
home-manager service always activate the default `polarity`, so mango's
`autostart.scm` runs `theme-mode restore` on login.

Bound to `SUPER+SHIFT+t` in `dotfiles/.config/mango/config.conf`.

### The polarity file

`~/.local/share/sde/polarity` contains `dark` or `light` and a newline. It is
a home-manager symlink into the store, **replaced** on every toggle, so
watchers have to watch the directory `~/.local/share/sde/`, not the file:
a watch on the file is lost after the first swap. While the symlink is being
replaced the file can be missing for a moment; readers ignore that and keep the
current mode.

## Who reads what

| Program | Reads | Live? | How |
|---|---|---|---|
| GTK3 apps (Thunar…) | dconf `gtk-theme`, `icon-theme`, `cursor-theme` | yes | home-manager's gtk module writes them to dconf on activation |
| GTK4 / libadwaita | dconf `color-scheme` (only light/dark, no theme) | yes | via the settings portal |
| Firefox / Electron | `color-scheme` + GTK theme | yes | same |
| Qt apps | `QT_STYLE_OVERRIDE`, `QT_QPA_PLATFORMTHEME`, Kvantum config | **no** | env and style are read at start, restart the app |
| kitty | `~/.local/share/kitty/sde-theme.conf` | yes | `SIGUSR1` reload |
| waybar | `~/.config/waybar/style.css` | yes | `SIGUSR2` reload |
| neovim | polarity file | yes | `vim.uv` fs watch in `nvim/lua/sergio/themes.lua` |
| emacs | polarity file | yes | `file-notify` in `emacs/simple/lisp/appearance.el` |
| bash / fzf | polarity file | new shells | `dotfiles/.bashrc` |
| fuzzel | `~/.config/fuzzel/fuzzel.ini` | next launch | re-reads its config every time it opens |

### kitty

Kitty's config is plain dotfiles (`~/.config/kitty` is a symlink), so
home-manager cannot write into it. Instead `kitty.conf` includes, after its own
`theme.conf`:

```
include ${HOME}/.local/share/kitty/sde-theme.conf
```

`theming.nix` generates that file: the Stylix colors, the
`sde.theming.kitty.<mode>` theme, or an empty comment (so `theme.conf` wins).
It always exists, so the include never fails. On `SIGUSR1` kitty re-reads its
whole config, so running windows change colors; going back to an empty file
restores `theme.conf`. The light kitty themes set `env THEME_IS_LIGHT=1`, the
older env-based mechanism, still used as a fallback.

### waybar

`home/sergio/waybar-style.css` is the dark bar. In light mode `waybar.nix`
appends a block that overrides its base colors (bar background, text, workspace
buttons). State colors (critical battery, temperature…) are shared.

### fuzzel

The launcher (`SUPER+p` apps and PATH commands, `SUPER+d` apps) and the
session menu `fuzzelpoweroffmenu` (`SUPER+SHIFT+q`, waybar's power button).
`home/sergio/fuzzel.nix` has the layout and the menu; `theming.nix` sets the
font, the icon theme and colors: a dark or light palette plus the theme's
`accent`. With `stylix`, Stylix's own fuzzel target colors it instead.

### neovim and emacs

Both read the polarity at startup and watch `~/.local/share/sde/`. On a change
they set the mode and reload the theme of that mode:

- nvim: `COLORS = { dark = "vscode", light = "vscode" }` and
  `vim.o.background`. `:Theme <name>` changes the theme of the current mode.
- emacs: `ssm/themes` (`ef-elea-dark` / `ef-elea-light`).
  `M-x ssm/load-theme` changes the theme of the current mode.

The emacs watch lives inside emacs, so it works for the daemon and for
standalone frames alike; no `emacsclient` needed.

### `sde-polarity-watch`

A user service that watches the same directory and signals kitty and waybar
when the mode changes. It covers changes that do not go through `theme-mode`
(a switch that changes `polarity`, for instance). `theme-mode` signals them as
well; a second reload is harmless.

## Desktop environments

Plasma and GNOME manage GTK/Qt themes and cursors themselves and write their
own `gtk-3.0/settings.ini`, `gtk-4.0/settings.ini`, `.gtkrc-2.0` and
`kdeglobals`. `modules/kde.nix` and `modules/gnome.nix` set
`sde.desktop.fullEnvironment = true`, which turns `sde.theming.enable` off for
home-manager (`modules/sde/nixos/desktop.nix`).

Coming back from a Plasma session, those files exist and home-manager would
refuse to replace them, failing the switch. They are declared with
`force = true` so home-manager takes them back.

## Adding things

**A theme**: add an entry to `themes` in `theming.nix` with `accent`, `gtk`,
`icons`, `cursor` and `qt`, using `pick` for the parts that differ per mode. The option
type is built from the attribute names, nothing else to register. To check the
names a package installs:

```bash
ls $(nix build --no-link --print-out-paths .#nixosConfigurations.rx.pkgs.<pkg>)/share/{themes,icons,Kvantum}
```

**A program that follows the mode**: read `~/.local/share/sde/polarity` at
start; to be live, watch `~/.local/share/sde/` (inotify) and reload on change.
If the program cannot read a file but can be told to reload (a signal, an IPC
call), add it to `sde-polarity-watch` instead.

## Open ends

- **Qt apps** need a restart to change mode.
- **gruvbox / nordic** have no light Kvantum theme in nixpkgs; their light Qt
  follows the GTK theme.
- The standalone `homeManagerConfigurations.sergio` (`make user`) does not pass
  `sde` and `inputs` as `extraSpecialArgs` and does not evaluate.
