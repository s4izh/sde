-- editing basics
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- number column
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"

vim.g.editorconfig = true

-- annoying things
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true

vim.opt.scrolloff = 8
vim.o.mouse = "a"

vim.opt.splitright = true -- place new window to right of current one
vim.opt.splitbelow = true -- place new window below the current one

vim.opt.wrap = false

-- search
vim.opt.hlsearch = true -- :noh to stop highlighting
vim.opt.incsearch = true

vim.o.path = vim.o.path .. "**"

-- disable autocomment on next line
-- vim.cmd[[
--     autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
-- ]]
vim.api.nvim_create_autocmd({"FileType"}, {
  pattern = {"*"},
  command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o",
})

vim.opt.tags="./tags;tags"
