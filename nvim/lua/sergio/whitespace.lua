vim.opt.list = true

vim.opt.listchars = {
  tab = '> ',      -- An arrow-like feel
  trail = '-',     -- A simple dash
  extends = '>',
  precedes = '<',
}

local whitespace_group = vim.api.nvim_create_augroup("WhitespaceToggle", { clear = true })

-- Hide whitespace in Insert mode, show it again on leaving Insert
vim.api.nvim_create_autocmd("InsertEnter", {
  group = whitespace_group,
  pattern = "*",
  command = "set nolist",
})
vim.api.nvim_create_autocmd("InsertLeave", {
  group = whitespace_group,
  pattern = "*",
  command = "set list",
})

-- <leader>tW: toggle whitespace visibility (<leader>tw is the wrap toggle)
vim.keymap.set('n', '<leader>tW', ':set list!<CR>', { desc = 'Toggle whitespace visibility' })

vim.api.nvim_create_user_command('TrimWhitespace', function()
  local save_cursor = vim.fn.getpos(".")
  vim.cmd([[%s/\s\+$//e]])
  vim.fn.setpos(".", save_cursor)
end, { desc = 'Trim trailing whitespace' })

-- Trim trailing whitespace on save
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  callback = function()
    local save_cursor = vim.fn.getpos(".")
    vim.cmd([[%s/\s\+$//e]])
    vim.fn.setpos(".", save_cursor)
  end,
})
