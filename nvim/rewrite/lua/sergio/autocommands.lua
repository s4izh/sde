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
