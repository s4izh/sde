local yank_group = vim.api.nvim_create_augroup('HighlightYank', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  group = yank_group,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank({
      higroup = 'IncSearch',
      timeout = 40,
    })
  end,
  desc = "Highlight text when is yanked"
})

-- ensure formatoptions are set
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "*",
--   callback = function()
--     vim.bo.formatoptions:remove({ "c", "r", "o" })
--   end,
--   desc = "Disable automatic comment continuation",
-- })

vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "*" },
  command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o",
})

-- return cursor to where it was last time the file was closed
local lastplace_group = vim.api.nvim_create_augroup("Lastplace", { clear = true })
vim.api.nvim_create_autocmd("BufWinEnter", {
  group = lastplace_group,
  pattern = "*",
  desc = "restore last cursor position",
  command = 'silent! normal! g`"zv',
})

-- disable conceal in insert mode for prose filetypes
local concealing_group = vim.api.nvim_create_augroup("Concealing", { clear = true })
vim.api.nvim_create_autocmd({ "InsertEnter", "InsertLeave" }, {
  group = concealing_group,
  pattern = { "*.md", "*.tex", "*.latex" },
  callback = function()
    vim.opt_local.conceallevel = 0
  end,
})
