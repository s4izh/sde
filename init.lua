vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn="no"

-- require('lualine').setup()

vim.cmd([[
  autocmd BufNewFile,BufRead ~/.config/sway/config.d/* set filetype=swayconfig
]])
