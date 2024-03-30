-- general
vim.keymap.set("n", "<leader>e", vim.cmd.Ex)
vim.keymap.set('n', "<leader>fv", ":find *")

-- clipboard shortcuts
vim.keymap.set("x", "<leader>p", [["_dP]])
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- delete without saving to any register
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])

-- nop
vim.keymap.set("n", "Q", "<nop>")

-- quickfix mappings
vim.keymap.set("n", "<C-n>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-p>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>q", "<cmd>copen<CR>")

-- move lines up and down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- centre screen after page up/down
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- centre screen after moving through search
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")
vim.keymap.set("n", "<leader>n", ":noh<cr>")

-- keep cursor on the same line on J
vim.keymap.set("n", "J", "mzJ`z")

-- custom scripts
vim.keymap.set("n", "<Leader>c", ":w | !compiler %<cr>")
vim.keymap.set("n", "<Leader>o", ":!opout %<cr><cr>")

-- conceallevel
vim.keymap.set("n", "<Leader>wc0", ":set conceallevel=0<cr>")
vim.keymap.set("n", "<Leader>wc2", ":set conceallevel=2<cr>")

-- resource config
vim.keymap.set("n", "<Leader>V", ":source ~/.config/nvim/init.lua <cr>")

-- diff config
if vim.opt.diff:get() then
  -- Map <leader>1 to :diffget LOCAL<CR>
  vim.api.nvim_set_keymap('n', '<leader>1', ':diffget LOCAL<CR>', { noremap = true, silent = true })
  -- Map <leader>2 to :diffget BASE<CR>
  vim.api.nvim_set_keymap('n', '<leader>2', ':diffget BASE<CR>', { noremap = true, silent = true })
  -- Map <leader>3 to :diffget REMOTE<CR>
  vim.api.nvim_set_keymap('n', '<leader>3', ':diffget REMOTE<CR>', { noremap = true, silent = true })
end

vim.keymap.set("n", "<Leader>R", ":set wrap! linebreak!<cr>")
