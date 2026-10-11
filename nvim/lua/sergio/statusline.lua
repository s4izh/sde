-- mini.statusline: minimal statusline that follows the active colorscheme
vim.pack.add({
  -- icon provider, used by mini.statusline (and picked up by oil.nvim too)
  { src = "https://github.com/nvim-mini/mini.icons" },
  { src = "https://github.com/nvim-mini/mini.statusline" },
})

require("mini.icons").setup()

-- let plugins that ask for nvim-web-devicons use mini.icons instead
MiniIcons.mock_nvim_web_devicons()

require("mini.statusline").setup({
  use_icons = true,
})
