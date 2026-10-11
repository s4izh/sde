vim.opt.termguicolors = true
vim.opt.background = "dark"

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

local CURRENT_COLOR = "vscode"

function M.standard(color)
  vim.cmd.colorscheme(color or CURRENT_COLOR)
end

function M.transparent(color)
  vim.cmd.colorscheme(color or CURRENT_COLOR)
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
  vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "white", bg = "none" })
  vim.api.nvim_set_hl(0, "LineNr", { bg = "none", ctermbg = "none", fg = "grey" })
  vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
  vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none", fg = "grey" })
  vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })
  vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none", fg = "grey" })
end

M.transparent()

local is_transparent = true
vim.keymap.set("n", "<leader>tt", function()
  is_transparent = not is_transparent
  if is_transparent then
    M.transparent()
  else
    M.standard()
  end
end, { desc = "toggle transparent background" })

vim.api.nvim_create_user_command("Theme", function(opts)
  if is_transparent then
    M.transparent(opts.args)
  else
    M.standard(opts.args)
  end
  CURRENT_COLOR = opts.args
end, {
  nargs = 1,
  complete = function(arg_lead)
    return vim.fn.getcompletion(arg_lead, "color")
  end,
})

return M
