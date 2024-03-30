-- editing basics
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- numbers
vim.opt.number = true
vim.opt.relativenumber = true

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
