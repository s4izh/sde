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

vim.opt.cursorline = false

vim.g.editorconfig = true

-- project-local config (.nvim.lua / .nvimrc / .exrc); gated by the trust db + secure
vim.o.exrc = true
vim.o.secure = true

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

-- vim.opt.formatoptions:remove({ "c", "r", "o" }) -- moved to autocommands.lua

-- colorcolumn: on/off at startup and column in config.lua
local colorcolumn = require("sergio.config").colorcolumn
vim.opt.colorcolumn = colorcolumn.enabled and tostring(colorcolumn.column) or ""

vim.o.path = vim.o.path .. "**"
vim.opt.tags = "./tags;tags"
