vim.g.mapleader = " "

require("sergio")

-- vim.opt.signcolumn="no"

-- require('lualine').setup()

require('gitsigns').setup()

vim.cmd([[
  autocmd BufNewFile,BufRead ~/.config/sway/config.d/* set filetype=swayconfig
  autocmd BufNewFile,BufRead ~/.dotfiles/.config/sway/config.d/* set filetype=swayconfig
]])

vim.cmd([[
  TransparentEnable
]])

local augroup = vim.api.nvim_create_augroup
local SergioGroup = augroup('Sergio', {})

local autocmd = vim.api.nvim_create_autocmd
local yank_group = augroup('HighlightYank', {})

-- function R(name)
--     require("plenary.reload").reload_module(name)
-- end

-- highlight when yanking text
autocmd('TextYankPost', {
    group = yank_group,
    pattern = '*',
    callback = function()
        vim.highlight.on_yank({
            higroup = 'IncSearch',
            timeout = 40,
        })
    end,
})

-- remove trailing whitespace on save
autocmd({"BufWritePre"}, {
    group = SergioGroup,
    pattern = "*",
    command = [[%s/\s\+$//e]],
})
