vim.opt.termguicolors = true

-- light/dark comes from ~/.local/share/sde/polarity, written by the nix
-- theming (theme-mode in nixos/home/sergio/theming.nix). THEME_IS_LIGHT from
-- the kitty theme is the fallback when the file is not there.
local polarity_file = vim.g.sde_polarity_file
  or (vim.env.XDG_DATA_HOME or vim.fn.expand("~/.local/share")) .. "/sde/polarity"

-- nil when the file is missing or half written, home-manager swaps the
-- symlink on every toggle
local function read_polarity()
  local f = io.open(polarity_file)
  if not f then
    return nil
  end
  local p = f:read("*l")
  f:close()
  if p == "light" or p == "dark" then
    return p
  end
end

vim.opt.background = read_polarity() or (vim.env.THEME_IS_LIGHT == "1" and "light" or "dark")

vim.pack.add({
  { src = "https://github.com/rose-pine/neovim", name = "rose-pine" },
  { src = "https://github.com/nordtheme/vim", name = "nord" },
  { src = "https://github.com/morhetz/gruvbox" },
  { src = "https://github.com/Carcuis/darcula.nvim" },
  { src = "https://github.com/savq/melange-nvim" },
  { src = "https://github.com/rebelot/kanagawa.nvim" },
  { src = "https://github.com/folke/tokyonight.nvim" },
  { src = "https://github.com/sainnhe/everforest" },
  { src = "https://github.com/blazkowolf/gruber-darker.nvim" },
  { src = "https://github.com/vague2k/vague.nvim" },
  { src = "https://github.com/miikanissi/modus-themes.nvim" },
  { src = "https://github.com/aktersnurra/no-clown-fiesta.nvim" },
  { src = "https://github.com/projekt0n/github-nvim-theme" },
  { src = "https://github.com/navarasu/onedark.nvim" },
  { src = "https://github.com/Mofiqul/vscode.nvim" },
  { src = "https://github.com/maxmx03/solarized.nvim" },
})

vim.g.gruvbox_contrast_dark = "hard"
require("rose-pine").setup({
  variant = "auto",
  dark_variant = "main",
  styles = {
    italic = false,
  },
})

local M = {}

-- colorscheme per background, :Theme changes the one of the current mode.
-- vscode follows 'background', so the same name works for both
local COLORS = { dark = "vscode", light = "vscode" }

local is_transparent = true

function M.standard(color)
  vim.cmd.colorscheme(color or COLORS[vim.o.background])
end

function M.transparent(color)
  local fg = vim.o.background == "dark" and "white" or "black"
  vim.cmd.colorscheme(color or COLORS[vim.o.background])
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
  vim.api.nvim_set_hl(0, "CursorLineNr", { fg = fg, bg = "none" })
  vim.api.nvim_set_hl(0, "LineNr", { bg = "none", ctermbg = "none", fg = "grey" })
  vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
  vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none", fg = "grey" })
  vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })
  vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none", fg = "grey" })
end

function M.apply(color)
  if is_transparent then
    M.transparent(color)
  else
    M.standard(color)
  end
end

M.apply()

-- watch the directory, not the file: the symlink gets replaced, which would
-- drop a watch on the file itself after the first toggle
local function watch_polarity()
  local dir = vim.fs.dirname(polarity_file)
  if not vim.uv.fs_stat(dir) then
    return
  end
  local watcher = vim.uv.new_fs_event()
  if not watcher then
    return
  end
  watcher:start(dir, {}, vim.schedule_wrap(function(err)
    if err then
      return
    end
    local p = read_polarity()
    if p and p ~= vim.o.background then
      vim.o.background = p
      M.apply()
    end
  end))
end

watch_polarity()

vim.keymap.set("n", "<leader>tt", function()
  is_transparent = not is_transparent
  M.apply()
end, { desc = "toggle transparent background" })

vim.api.nvim_create_user_command("Theme", function(opts)
  M.apply(opts.args)
  COLORS[vim.o.background] = opts.args
end, {
  nargs = 1,
  complete = function(arg_lead)
    return vim.fn.getcompletion(arg_lead, "color")
  end,
})

return M
