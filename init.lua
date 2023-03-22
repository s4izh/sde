vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn="no"

vim.cmd [[
        imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")
        let g:copilot_no_tab_map = v:true
]]


-- require('lualine').setup()
