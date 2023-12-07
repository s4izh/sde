vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn="no"

-- require('lualine').setup()

require('gitsigns').setup()
require('colorizer').setup()

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

autocmd({"BufWritePre"}, {
  group = SergioGroup,
  pattern = "*.rs",
  command = [[LspZeroFormat]],
})

autocmd({"BufWritePre"}, {
  group = SergioGroup,
  pattern = "*.go",
  command = [[LspZeroFormat]],
})

-- this is from Gavin Freeborn
vim.cmd([[
function! Statusline_expr()
	let mod  = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
	let ft   = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"
	let fug  = "%3*%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
	let job  = "%2*%{exists('g:job') ? '[Job Running!]' : ''}%*"
	let zoom = "%3*%{exists('t:maximize_hidden_save') ? '[Z]' : ''}%*"
	let sep  = ' %= '
	let pos  = ' %-14.(%l,%c%V%) '
	let pct  = ' %P'

	return '%<%f %<'.mod.fug.job.zoom.sep.pos.pct
endfunction
let &statusline = Statusline_expr()
set laststatus=2 "show statusbar
]])

-- vim.cmd([[highlight StatusLine ctermbg=NONE guibg=NONE]])

vim.g.slime_target = "tmux"

vim.cmd([[
  "let g:slime_paste_file = expand("$HOME/.slime_paste")
  let g:slime_paste_file = tempname()
]])

vim.g.tmux_navigator_no_mappings = 1

vim.cmd([[
let g:tmux_navigator_no_mappings = 1
noremap <silent> <M-h> :<C-U>TmuxNavigateLeft<cr>
noremap <silent> <M-j> :<C-U>TmuxNavigateDown<cr>
noremap <silent> <M-k> :<C-U>TmuxNavigateUp<cr>
noremap <silent> <M-l> :<C-U>TmuxNavigateRight<cr>
noremap <silent> <M-\> :<C-U>TmuxNavigatePrevious<cr>
]])

require('gen').model = 'codellama'
