vim.opt.guicursor = ""

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = true
-- :noh to stop highlighting
vim.opt.incsearch = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"

vim.opt.updatetime = 50

-- vim.opt.colorcolumn = "100"

-- vim.o.cursorline = true

vim.o.mouse = "a"

vim.opt.splitright = true -- Place new window to right of current one
vim.opt.splitbelow = true -- Place new window below the current one

-- disable autocomment on next line
vim.cmd[[
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
]]

-- Show endline characters
-- vim.opt.list = true
-- vim.opt.listchars:append("eol:$")

-- Show tab characters
-- vim.opt.listchars:append("tab:>>")

-- Show trailing whitespace

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

vim.g.editorconfig = true

vim.o.path = vim.o.path .. '**'
vim.o.wildignore = "*/**build**/*"
