vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn="no"

vim.cmd('command! -bang W lua SudoWrite("<bang>")')

-- require('lualine').setup()
