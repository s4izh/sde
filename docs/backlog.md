# Backlog

Things to do, roughly in priority order. Check items off as they land; move
anything that needs a design decision into an [ADR](adr/).

## Bugs

- [ ] **shell**: `.bashrc:41-45` sets `FZF_DEFAULT_OPTS=--color=light` and then
      overwrites it unconditionally with `--color=dark`. Use if/else.
- [ ] **nixos**: `hosts/vm` and `hosts/z390` import `${modules}/dwm.nix`, which moved
      to `modules/wm/dwm.nix`. Fix or delete those hosts.

## Shell cleanup

Real chain: `.bashrc` → `shell/bash_profile` → `functions`, `aliases`, `prompt`,
`completions`.

- [ ] `.bashrc:17`: `PS1` is dead, `prompt` overrides it via `PROMPT_COMMAND`.
- [ ] `.bashrc:33-36`: commented guix block.
- [ ] `shell/prompt:3-61`: `_set_prompt` is unused; keep only `_basic_prompt`.
- [ ] `shell/bash_profile:31-58`: nixos/guix branches that only `echo` to
      /dev/null. Keep the foreign-distro guix branch; define `GUIX_LOCPATH` once
      (it's also in `.bashrc:38`).
- [ ] `shell/bash_profile:8`: `export TERM=xterm-256color` overrides kitty/foot.
- [ ] `shell/aliases`: duplicated `grep` (13, 25) and `tree` (29, 31) aliases.
- [ ] `claude-personal` alias duplicates the `claude` script. Keep one.
- [ ] `shell/inputrc`: drop commented settings; add `inputrc:.inputrc` to
      `config.mk` (current link was made by hand).
- [ ] Archive `dotfiles/.bash_aliases` (Arch-only, not linked); move `psmem`/`df -h`
      to `shell/aliases` if wanted.
- [ ] Delete `guix/modules/sergio/home/{.bashrc,.bash_profile,.profile}` and the
      commented block in `config.scm:72-84`.
- [ ] Rewrite `ubuntu/guix/.bashrc` and `wsl/.bashrc` as thin wrappers that source
      `~/.config/shell/*` plus host-specific lines (both are 100+ lines of
      Debian boilerplate duplicating the shared config).

## Neovim cleanup

- [ ] `vim.pack.del({'denote.nvim', 'modeline.nvim'})`: pinned in
      `nvim-pack-lock.json` but no longer added.
- [ ] Delete `nvim/utils/plugin_add` (pre-lazy submodule installer).
- [ ] `plugins.lua:23`: comment points to `colors.lua`; it's `themes.lua`.
- [ ] `themes.lua`: 16 colorscheme plugins installed; trim to the ones used.
- [ ] Decide native completion vs `cmp.lua`. If native: delete `cmp.lua`, the
      branch in `lsp.lua:135` and the note in `completions.lua:6`.
- [ ] Decide on `statusline.lua` (unused, installs mini.*). Delete if default
      statusline stays.
- [ ] `colors/`: delete `ziggy.lua`, `hipster.vim`, and one of
      `my-spaceway.lua`/`spaceway2.lua`.
- [ ] `plugin/tmux_navigator.vim` is vendored; replace with a `vim.pack` entry or
      drop the no-op `g:tmux_navigator_no_mappings` in `after/plugin`.
- [ ] `after/ftplugin/lua.vim` (only comments), `c.vim`/`cpp.vim` (set the
      default `commentstring`).
- [ ] `context.lua:55-56`: leftover commented require.
- [ ] `nixos/pkgs/nvim-mnw.nix` (WIP): fix `vimPlugins`/`withAllGrammars`
      casing and wire it into the flake, or delete.
- [ ] Write ADR for the lazy.nvim → `vim.pack` migration.

## Emacs cleanup

`~/.emacs.d` → `emacs/simple`. `minimal/` and `old/` are archived.

- [ ] Bring the useful parts of minimal-emacs.d (early-init tweaks, defaults)
      into `simple/` without depending on the framework.
- [ ] `simple/lisp/evil-setup.el`: only referenced by a commented load. Delete or
      document it as a toggle.
- [ ] Commented code: `completion.el:53-58`, `keybindings.el:38-49`,
      `writing.el:37-43, 59`.
- [ ] `backup-directory-alist` writes notes backups into `simple/backups/` inside
      the repo; move it out.

## Dotfiles cleanup

Done on 2026-10-11: unused configs and ~90 scripts moved to `.archive/` (see
`.archive/README.md`).

- [ ] mango: `config2.conf`, `scripts/autostart.{lua,sh}` (not sourced).
- [ ] `modules/wm/mangowc.nix`: `dgop` was for DMS, now archived.
- [ ] X11 configs are kept but their scripts are archived: sxhkd and dwm
      bindings to `dmenu*`, `statusbar/` etc. are broken until restored.
- [ ] Arch-era tooling: `MakefileDots`, `MakefileOld`, `install`, `Dockerfile`,
      `archlinux/`, `ubuntu/`, `etc/pacman.conf`.
- [ ] `alacritty/yml/` (pre-0.13 format) and `alacritty/utils/migrate`.
- [ ] `waybar/`: check which of `layout.py`/`layout.scm`/`layout_mapper.sh` is
      used.
- [ ] `claude-work:2`: comment says "personal" config dir.

## NixOS

- [ ] `flake.nix` references `./overlays` and `./modules/home-manager`, which
      don't exist (only works because nothing evaluates them).
- [ ] `modules/sde/nixos/ollama.nix`: options copy-pasted from virtualisation
      (`podman`, `libvirt`, "Docker virtualization").
- [ ] Unify the two module styles (`modules/*.nix` imports vs `sde.*` options),
      or document when to use each in an ADR.
- [ ] `parts/`: finish the flake-parts migration or delete it.
- [ ] Remove `hosts/vm`, `z390`, `thinkcenter` if they won't come back.
- [ ] Delete the local `panic-20260313-0929` branch.
