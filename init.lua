vim.g.mapleader = " "
vim.opt.guicursor = ""

require("sergio")

-- highlight when yanking text
local yank_group = vim.api.nvim_create_augroup('HighlightYank', {})
vim.api.nvim_create_autocmd('TextYankPost', {
  group = yank_group,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank({
      higroup = 'IncSearch',
      timeout = 40,
    })
  end,
})

vim.api.nvim_set_option("grepprg", "rg --vimgrep --smart-case")
vim.api.nvim_set_option("grepformat", "%f:%l:%c:%m,%f:%l:%m")
