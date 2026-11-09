vim.opt.relativenumber = false
vim.opt.number = false

vim.keymap.set('n', 'H', '<cmd>colder<cr>', { silent = true, buffer = true, desc = "Older quickfix list" })
vim.keymap.set('n', 'L', '<cmd>cnewer<cr>', { silent = true, buffer = true, desc = "Newer quickfix list" })
