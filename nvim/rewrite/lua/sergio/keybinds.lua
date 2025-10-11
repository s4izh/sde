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

-- keep cursor on the same line on J
vim.keymap.set("n", "J", "mzJ`z")

-- custom scripts
vim.keymap.set("n", "<leader>cc", ":w | !compiler %<cr>")
vim.keymap.set("n", "<leader>O", ":!opout %<cr><cr>")
vim.keymap.set("n", "<leader>X", ":!chmod +x %<cr><cr>")

vim.keymap.set("v", "<leader><Leader>x", ":source %<cr>")
vim.keymap.set("n", "<leader>x", ":.lua<cr>")
vim.keymap.set("v", "<leader>x", ":lua<cr>")

-- conceallevel
vim.keymap.set("n", "<leader>wc0", ":set conceallevel=0<cr>")
vim.keymap.set("n", "<leader>wc2", ":set conceallevel=2<cr>")

-- substution sugar
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
vim.keymap.set("v", '<leader>s', '"hy:%s/<C-r>h//gc<left><left><left>', { noremap = true, silent = true })

-- resource config
vim.keymap.set("n", "<leader>V", ":source ~/.config/nvim/init.lua <cr>")

-- writing
vim.keymap.set("n", "<leader>wsc", ":setlocal spell spelllang=ca<cr>")
vim.keymap.set("n", "<leader>wse", ":setlocal spell spelllang=es<cr>")
vim.keymap.set("n", "<leader>wsi", ":setlocal spell spelllang=en<cr>")
vim.keymap.set("n", "<leader>wS", ":setlocal nospell<cr>")
vim.keymap.set("n", "<leader>wT", ":r!~/notes/utils/template %<cr>ggdd")
vim.keymap.set("n", "<leader>wl", ":r!~/notes/utils/link-note % 2>/dev/null<cr>")

vim.keymap.set("n", "<leader>fw", ":silent grep <C-R><C-W> | copen<cr>")

-- TOGGLES

local toggles = require("sergio.toggles")

-- linenumber configs toggle
vim.keymap.set("n", "<leader>tcc", toggles.toggle_color_column, { noremap = true, silent = true, desc = "toggle color column" })
vim.keymap.set("n", "<leader>tla", toggles.toggle_line_numbers, { noremap = true, silent = true, desc = "toggle line numbers column" })
vim.keymap.set("n", "<leader>tlr", ":set relativenumber!<cr>", { noremap = true, silent = true, desc = "toggle relative number" })

-- line wrapping
vim.keymap.set("n", "<leader>tw", ":set wrap! linebreak!<cr>"color )

-- search highlights
vim.keymap.set("n", "<leader>th", ":noh<cr>")

-- cursor
vim.keymap.set("n", "<leader>tcC", ":set cursorcolumn!<cr>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tcl", ":set cursorline!<cr>", { noremap = true, silent = true })

-- vim.keymap.set("n", "<leader>tcl", ":set cursorline!<cr>", { noremap = true, silent = true })
