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

local lastplace_group = vim.api.nvim_create_augroup('Lastplace', {})
vim.api.nvim_create_autocmd({'BufWinEnter'}, {
  group = lastplace_group,
  desc = 'return cursor to where it was last time closing the file',
  pattern = '*',
  -- there is a mark at '"' register
  command = 'silent! normal! g`"zv',
})

vim.api.nvim_set_option("grepprg", "rg --vimgrep --smart-case --hidden")
vim.api.nvim_set_option("grepformat", "%f:%l:%c:%m,%f:%l:%m")

vim.g.git_base = "HEAD"
