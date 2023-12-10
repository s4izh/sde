vim.g.mapleader = " "

require("sergio")

vim.opt.signcolumn = "no"

vim.cmd([[
  autocmd BufNewFile,BufRead ~/.config/sway/config.d/* set filetype=swayconfig
  autocmd BufNewFile,BufRead ~/.dotfiles/.config/sway/config.d/* set filetype=swayconfig
]])

local augroup = vim.api.nvim_create_augroup
local sergio_group = augroup('Sergio', {})

local autocmd = vim.api.nvim_create_autocmd
local yank_group = augroup('HighlightYank', {})

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
autocmd({ "BufWritePre" }, {
  group = sergio_group,
  pattern = "*",
  command = [[%s/\s\+$//e]],
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


-- vim.g.tmux_navigator_no_mappings = 1

-- vim.cmd([[
-- let g:tmux_navigator_no_mappings = 1
-- noremap <silent> <M-h> :<C-U>TmuxNavigateLeft<cr>
-- noremap <silent> <M-j> :<C-U>TmuxNavigateDown<cr>
-- noremap <silent> <M-k> :<C-U>TmuxNavigateUp<cr>
-- noremap <silent> <M-l> :<C-U>TmuxNavigateRight<cr>
-- noremap <silent> <M-\> :<C-U>TmuxNavigatePrevious<cr>
-- ]])
