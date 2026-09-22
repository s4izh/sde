vim.opt.relativenumber = false
vim.opt.number = false

vim.keymap.set('n', 'H', '<cmd>colder<cr>', { silent = true, buffer = true })
vim.keymap.set('n', 'L', '<cmd>cnewer<cr>', { silent = true, buffer = true })
