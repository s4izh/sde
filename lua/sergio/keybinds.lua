vim.g.mapleader = " "
vim.keymap.set("n", "<leader>e", vim.cmd.Ex)

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")
vim.keymap.set("n", "<leader>n", ":noh<cr>")

-- vim.keymap.set("n", "<leader>vwm", function()
--     require("vim-with-me").StartVimWithMe()
-- end)
-- vim.keymap.set("n", "<leader>svwm", function()
--     require("vim-with-me").StopVimWithMe()
-- end)

-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])

vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.cmd [[ nnoremap S :%s//g<Left><Left> ]]
vim.cmd [[ vnoremap S :s//g<Left><Left> ]]

-- extra
vim.keymap.set("n", "<Leader>z", "<cmd>Goyo<cr>")

-- vim.keymap.set("n", "<Leader>Z", "<cmd>!zathura <c-r><c-p> &<cr>")

vim.keymap.set("n", "<Leader>o", ":!xdg-open <c-r><c-p> &<cr>")
vim.keymap.set("n", "<Leader>L", ":!alacritty -e lfuni % &<cr>")

vim.keymap.set("n", "<Leader>r", ":set wrap! linebreak!<cr>")

vim.keymap.set("n", "<Leader>c", ":w | !compiler %<cr>")
vim.keymap.set("n", "<Leader>O", ":!opout %<cr><cr>")

-- vim.keymap.set("n", "<Leader>wb", ":!~/vimwiki/scripts/backlinks.sh %<cr>")

vim.keymap.set("n", "<Leader>la", ":LspStop<cr>")

-- vim.keymap.set("n", "H", ":bp<cr>")
-- vim.keymap.set("n", "L", ":bn<cr>")

-- function ToggleColorColumn()
--   if vim.wo.colorcolumn == "" then
--     vim.wo.colorcolumn = "80";
--   else
--     vim.wo.colorcolumn = "";
--   end
-- end

vim.keymap.set("n", "<Leader>T", ":Rg :")

vim.keymap.set("n", "<Leader>Ws", ":w ~/.local/scripts/")

vim.keymap.set("n", "<Leader>ir", ":norm i$\\rightarrow$<cr>")
vim.keymap.set("n", "<Leader>is", ":r! wayss -w <cr>")

vim.keymap.set("n", "<Leader>Ce", ":set colorcolumn=80<cr>")
vim.keymap.set("n", "<Leader>Ca", ":set colorcolumn=0<cr>")

-- Define a function to open the PDF in Zathura
-- function Open_pdf()
--   local filename = vim.fn.expand('%:t:r')
--   local pdf_path = vim.fn.expand('%:p:h') .. '/' .. filename .. '.pdf'
--   vim.fn.system('zathura ' .. pdf_path)
-- end

-- Bind the function to a key
-- vim.api.nvim_set_keymap('n', '<leader>p', ':lua open_pdf()<CR>', { noremap = true, silent = true })

