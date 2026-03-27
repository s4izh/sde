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

local concealing_group = vim.api.nvim_create_augroup("Concealing", { clear = true })

-- vim.api.nvim_create_autocmd("InsertEnter", {
--   group = concealing_group,
--   pattern = { "*.md", "*.tex", "*.latex" },
--   command = "setlocal conceallevel=0",
--   -- callback = function()
--   --   vim.opt_local.conceallevel = 0
--   -- end,
-- })
-- vim.api.nvim_create_autocmd("InsertLeave", {
--   group = concealing_group,
--   pattern = { "*.md", "*.tex", "*.latex" },
--   command = "setlocal conceallevel=2",
-- })

vim.api.nvim_create_autocmd({ "InsertEnter", "InsertLeave" }, {
  group = concealing_group,
  pattern = { "*.md", "*.tex", "*.latex" },
  callback = function(args)
    if args.event == "InsertEnter" then
      vim.opt_local.conceallevel = 0
    else
      vim.opt_local.conceallevel = 0
    end
  end,
})
