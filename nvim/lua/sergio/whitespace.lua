vim.opt.list = true

vim.opt.listchars = {
  tab = '» ',       -- Show tabs as »
  trail = '·',      -- Show trailing spaces as dots
  nbsp = '␣',       -- Show non-breaking spaces
  extends = '→',    -- Show character when line continues off-screen
  precedes = '←',   -- Show character when line continues off-screen
}

vim.opt.listchars = {
  tab = '> ',      -- An arrow-like feel
  trail = '-',     -- A simple dash
  extends = '>',
  precedes = '<',
}

-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "make",
--   callback = function()
--     vim.opt_local.list = true
--     vim.opt_local.listchars = {
--       tab = '> ',    -- Very obvious arrow for required Tabs
--       trail = '-',   -- Dot for accidental trailing spaces
--       nbsp = '␣',
--     }
--   end,
-- })

local whitespace_group = vim.api.nvim_create_augroup("WhitespaceToggle", { clear = true })

-- Hide whitespaces in Insert mode
vim.api.nvim_create_autocmd("InsertEnter", {
  group = whitespace_group,
  pattern = "*",
  command = "set nolist",
})
-- Show whitespaces when leaving Insert mode
vim.api.nvim_create_autocmd("InsertLeave", {
  group = whitespace_group,
  pattern = "*",
  command = "set list",
})

vim.keymap.set('n', '<leader>tw', ':set list!<CR>', { desc = 'Toggle whitespace visibility' })

vim.api.nvim_create_user_command('TrimWhitespace', function()
    local save_cursor = vim.fn.getpos(".")
    -- Substitution command: %s/\s\+$//e
    -- %s = entire file
    -- \s\+ = one or more whitespace characters
    -- $ = at the end of a line
    -- /e = don't flag an error if no match is found
    vim.cmd([[%s/\s\+$//e]])
    vim.fn.setpos(".", save_cursor)
end, { desc = 'Trim trailing whitespace' })

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    callback = function()
        local save_cursor = vim.fn.getpos(".")
        vim.cmd([[%s/\s\+$//e]])
        vim.fn.setpos(".", save_cursor)
    end,
})

