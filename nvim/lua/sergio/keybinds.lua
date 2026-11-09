local keymaps = require("sergio.keymaps")
keymaps.group("<leader>c", "+compile / AI context")
keymaps.group("<leader>t", "+toggles")
keymaps.group("<leader>tc", "+columns / cursor")
keymaps.group("<leader>tl", "+line numbers")
keymaps.group("<leader>w", "+writing")
keymaps.group("<leader>x", "+execute (Lua)")
keymaps.group("<leader>wc", "+conceal")
keymaps.group("<leader>ws", "+spell")

-- general
vim.keymap.set("n", "<leader>e", vim.cmd.Ex, { desc = "Open netrw (:Ex)" })
vim.keymap.set('n', "<leader>fv", ":find *", { desc = "Find file (:find *)" })

-- clipboard shortcuts
vim.keymap.set("x", "<leader>p", [["_dP]], { desc = "Paste over selection, keep register" })
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to system clipboard" })
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Yank line to system clipboard" })
-- delete without saving to any register
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

-- nop
vim.keymap.set("n", "Q", "<nop>", { desc = "Disabled (no Ex mode)" })

-- quickfix mappings
vim.keymap.set("n", "<C-n>", "<cmd>cnext<CR>zz", { desc = "Next quickfix item" })
vim.keymap.set("n", "<C-p>", "<cmd>cprev<CR>zz", { desc = "Previous quickfix item" })
-- vim.keymap.set("n", "<leader>q", "<cmd>copen<CR>")

-- move lines up and down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- centre screen after page up/down
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Half page down, centred" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Half page up, centred" })

-- centre screen after moving through search
vim.keymap.set("n", "n", "nzzzv", { desc = "Next match, centred" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "Previous match, centred" })

-- keep cursor on the same line on J
vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines, keep cursor" })

-- custom scripts
vim.keymap.set("n", "<leader>cc", ":w | !compiler %<cr>", { desc = "Save and run compiler on file" })
vim.keymap.set("n", "<leader>O", ":!opout %<cr><cr>", { desc = "Open compiled output (opout)" })
vim.keymap.set("n", "<leader>X", ":!chmod +x %<cr><cr>", { desc = "Make file executable" })

vim.keymap.set("n", "<leader>xf", ":source %<cr>", { desc = "Source current file" })
vim.keymap.set("n", "<leader>xl", ":.lua<cr>", { desc = "Run current line as Lua" })
vim.keymap.set("v", "<leader>xs", ":lua<cr>", { desc = "Run selection as Lua" })

-- conceallevel
vim.keymap.set("n", "<leader>wc0", ":set conceallevel=0<cr>", { desc = "Conceal off (conceallevel=0)" })
vim.keymap.set("n", "<leader>wc2", ":set conceallevel=2<cr>", { desc = "Conceal on (conceallevel=2)" })

-- substution sugar
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Substitute word under cursor" })
vim.keymap.set("v", '<leader>s', '"hy:%s/<C-r>h//gc<left><left><left>', { noremap = true, silent = true, desc = "Substitute selection (confirm)" })

-- resource config
vim.keymap.set("n", "<leader>V", ":source ~/.config/nvim/init.lua <cr>", { desc = "Reload nvim config" })

-- writing
vim.keymap.set("n", "<leader>wsc", ":setlocal spell spelllang=ca<cr>", { desc = "Spell check: Catalan" })
vim.keymap.set("n", "<leader>wse", ":setlocal spell spelllang=es<cr>", { desc = "Spell check: Spanish" })
vim.keymap.set("n", "<leader>wsi", ":setlocal spell spelllang=en<cr>", { desc = "Spell check: English" })
vim.keymap.set("n", "<leader>wS", ":setlocal nospell<cr>", { desc = "Spell check off" })
vim.keymap.set("n", "<leader>wT", ":r!~/notes/utils/template %<cr>ggdd", { desc = "Insert note template" })
vim.keymap.set("n", "<leader>wl", ":r!~/notes/utils/link-note % 2>/dev/null<cr>", { desc = "Insert link to a note" })

vim.keymap.set("n", "<leader>fw", ":silent grep <C-R><C-W> | copen<cr>", { silent = true, desc = "Grep word under cursor (quickfix)" })

-- TOGGLES

local toggles = require("sergio.toggles")

-- linenumber configs toggle
vim.keymap.set("n", "<leader>tcc", toggles.toggle_color_column, { noremap = true, silent = true, desc = "Toggle color column" })
vim.keymap.set("n", "<leader>tla", toggles.toggle_line_numbers, { noremap = true, silent = true, desc = "Toggle line numbers" })
vim.keymap.set("n", "<leader>tlr", ":set relativenumber!<cr>", { noremap = true, silent = true, desc = "Toggle relative numbers" })

-- line wrapping
vim.keymap.set("n", "<leader>tw", ":set wrap! linebreak!<cr>", { desc = "Toggle line wrap" })

-- search highlights
vim.keymap.set("n", "<leader>th", ":noh<cr>", { desc = "Clear search highlight" })

-- cursor
vim.keymap.set("n", "<leader>tcC", ":set cursorcolumn!<cr>", { noremap = true, silent = true, desc = "Toggle cursor column" })
vim.keymap.set("n", "<leader>tcl", ":set cursorline!<cr>", { noremap = true, silent = true, desc = "Toggle cursor line" })

-- vim.keymap.set("n", "<leader>tcl", ":set cursorline!<cr>", { noremap = true, silent = true })
