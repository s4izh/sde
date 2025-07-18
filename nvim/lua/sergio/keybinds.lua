-- general
-- vim.keymap.set("n", "<leader>e", vim.cmd.Ex)
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
-- vim.keymap.set("n", "<leader>q", "<cmd>copen<CR>")

-- move lines up and down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- centre screen after page up/down
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- centre screen after moving through search
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")
vim.keymap.set("n", "<leader>th", ":noh<cr>")

-- keep cursor on the same line on J
vim.keymap.set("n", "J", "mzJ`z")

-- custom scripts
vim.keymap.set("n", "<Leader>cc", ":w | !compiler %<cr>")
vim.keymap.set("n", "<Leader>O", ":!opout %<cr><cr>")
vim.keymap.set("n", "<Leader>X", ":!chmod +x %<cr><cr>")

vim.keymap.set("v", "<Leader><Leader>x", ":source %<cr>")
vim.keymap.set("n", "<Leader>x", ":.lua<cr>")
vim.keymap.set("v", "<Leader>x", ":lua<cr>")

-- conceallevel
vim.keymap.set("n", "<Leader>wc0", ":set conceallevel=0<cr>")
vim.keymap.set("n", "<Leader>wc2", ":set conceallevel=2<cr>")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

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

-- writing
vim.keymap.set("n", "<Leader>wsc", ":setlocal spell spelllang=ca<cr>")
vim.keymap.set("n", "<Leader>wse", ":setlocal spell spelllang=es<cr>")
vim.keymap.set("n", "<Leader>wsi", ":setlocal spell spelllang=en<cr>")
vim.keymap.set("n", "<Leader>wS", ":setlocal nospell<cr>")
vim.keymap.set("n", "<Leader>wT", ":r!~/notes/utils/template %<cr>ggdd")
vim.keymap.set("n", "<Leader>wl", ":r!~/notes/utils/link-note % 2>/dev/null<cr>")


vim.keymap.set("n", "<Leader>fw", ":silent grep <C-R><C-W> | copen<cr>")
--vim.keymap.set("v", "<Leader>fs", ":grep -r <C-r>h | copen<cr>")

vim.keymap.set("n", "<Leader>r", ":luafile %<cr>")
vim.keymap.set("n", "<Leader>vc", ":e ~/.config/nvim/init.lua<cr>")

vim.keymap.set('v', '<C-r>', '"hy:%s/<C-r>h//gc<left><left><left>', { noremap = true, silent = true })

-- vim.keymap.set('v', '<Leader>fs', '"hy:silent grep <C-r>h | copen<CR>', { noremap = true, silent = true })
vim.keymap.set('v', '<Leader>fs', function()
  vim.cmd('normal! "hy')
  local query = vim.fn.getreg('h')
  query = vim.fn.escape(query, '\\')
  query = vim.fn.escape(query, '"')
  query = vim.fn.escape(query, '(')
  query = vim.fn.escape(query, ')')
  vim.cmd('silent grep "' .. query .. '" .')
  vim.cmd('copen')
end, { noremap = true, silent = true })


local function toggle_line_numbers()
  local is_number = vim.wo.number
  local is_relativenumber = vim.wo.relativenumber

  if is_number or is_relativenumber then
    vim.wo.number = false
    vim.wo.relativenumber = false
    print("Line numbers disabled")
  else
    vim.wo.number = true
    vim.wo.relativenumber = true
    print("Line numbers enabled")
  end
end

vim.keymap.set("n", "<Leader>tl", toggle_line_numbers, { noremap = true, silent = true })

vim.api.nvim_create_user_command("ToggleLineNumbers", function(opts)
  toggle_line_numbers()
end, {
  nargs = 0,
})
