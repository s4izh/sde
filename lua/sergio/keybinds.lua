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

vim.keymap.set("n", "H", ":bp<cr>");
vim.keymap.set("n", "L", ":bn<cr>");

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
-- vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
-- vim.keymap.set("n", "<C-s>", "<cmd>silent !tmux neww tmux-picker<CR>")

-- vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
-- vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>cprev<CR>zz")

vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.cmd [[ nnoremap S :%s//g<Left><Left> ]]
vim.cmd [[ vnoremap S :s//g<Left><Left> ]]

-- extra
vim.keymap.set("n", "<Leader>z", "<cmd>Goyo<cr>")

-- vim.keymap.set("n", "<Leader>Z", "<cmd>!zathura <c-r><c-p> &<cr>")

vim.keymap.set("n", "<Leader>o", ':!sh -c "setsid xdg-open <c-r><c-p>"<cr><cr>')
vim.keymap.set("n", "<Leader>L", ":!alacritty -e lfuni % &<cr>")

vim.keymap.set("n", "<Leader>R", ":set wrap! linebreak!<cr>")

vim.keymap.set("n", "<Leader>c", ":w | !compiler %<cr>")
vim.keymap.set("n", "<Leader>O", ":!opout %<cr><cr>")

-- vim.keymap.set("n", "<Leader>wb", ":!~/vimwiki/scripts/backlinks.sh %<cr>")

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

-- WRITTING
vim.keymap.set("n", "<Leader>wsc", ":setlocal spell spelllang=ca<cr>")
vim.keymap.set("n", "<Leader>wse", ":setlocal spell spelllang=es<cr>")
vim.keymap.set("n", "<Leader>wsi", ":setlocal spell spelllang=en<cr>")
vim.keymap.set("n", "<Leader>wS", ":setlocal nospell<cr>")
vim.keymap.set("n", "<Leader>wT", ":r!~/notes/src/template %<cr>ggdd")
-- vim.keymap.set("n", "<Leader>wn", ":set nospell")

vim.keymap.set("n", "<Leader>wf", ":set ft=")

vim.keymap.set("n", "<Leader>wt", ":Toch<cr>")

-- resumen especifícos
-- vim.keymap.set("n", "<Leader>rs", ":r! wayss -a <cr>")

vim.keymap.set("n", "<Leader>wc0", ":set conceallevel=0<cr>")
vim.keymap.set("n", "<Leader>wc2", ":set conceallevel=2<cr>")

-- already set in tmux config
vim.keymap.set("i", "<C-g>", "<esc>")
vim.keymap.set("n", "<C-g>", "<esc>")
vim.keymap.set("v", "<C-g>", "<esc>")

-- vim.keymap.set("i", "kj", "<esc>")


-- Check if we're in a diff window
if vim.opt.diff:get() then
  -- Map <leader>1 to :diffget LOCAL<CR>
  vim.api.nvim_set_keymap('n', '<leader>1', ':diffget LOCAL<CR>', { noremap = true, silent = true })

  -- Map <leader>2 to :diffget BASE<CR>
  vim.api.nvim_set_keymap('n', '<leader>2', ':diffget BASE<CR>', { noremap = true, silent = true })

  -- Map <leader>3 to :diffget REMOTE<CR>
  vim.api.nvim_set_keymap('n', '<leader>3', ':diffget REMOTE<CR>', { noremap = true, silent = true })
end

vim.keymap.set("n", "<Leader>V", ":source ~/.config/nvim/init.lua <cr>")

vim.keymap.set("n", "<Leader>CC", ":!shellcheck %<cr>")

vim.keymap.set("n", "<Leader>db", ":DapToggleBreakpoint <cr>")
vim.keymap.set("n", "<Leader>dr", ":DapContinue <cr>")
vim.keymap.set("n", "<Leader>ds", ":DapStepOver <cr>")
vim.keymap.set("n", "<Leader>di", ":DapStepInto <cr>")
vim.keymap.set("n", "<Leader>do", ":DapStepOut <cr>")

vim.keymap.set("n", "<Leader>r", "q:?Dispatch<cr><cr>")

vim.keymap.set("n", "<Leader>m", ":Make<cr>")

vim.keymap.set("n", "<Leader>gv", ":vs <cfile><cr>")
vim.keymap.set("n", "<Leader>gm", ":Man <c-r><c-w><cr>")

-- ident all
vim.keymap.set("n", "<Leader>I", "ggvG=<c-o>")

-- gen.nvim
vim.keymap.set('v', "<leader>]", ":Gen<CR>")
vim.keymap.set('n', "<leader>]", ":Gen<CR>")
