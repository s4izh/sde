vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn="no"

-- require('lualine').setup()

vim.cmd([[
  autocmd BufNewFile,BufRead ~/.config/sway/config.d/* set filetype=swayconfig
  autocmd BufNewFile,BufRead ~/.dotfiles/.config/sway/config.d/* set filetype=swayconfig
]])

vim.cmd([[
  TransparentEnable
]])
