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

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
-- vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50


-- vim.opt.colorcolumn = "80"

vim.o.mouse = "a"

vim.opt.splitright = true -- Place new window to right of current one
vim.opt.splitbelow = true -- Place new window below the current one

-- disable autocomment on next line
vim.cmd[[
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
]]

--Set completeopt to have a better completion experience
-- :help completeopt
-- menuone: popup even when there's only one match
-- noinsert: Do not insert text until a selection is made
-- noselect: Do not select, force to select one from the menu
-- shortness: avoid showing extra messages when using completion
-- updatetime: set updatetime for CursorHold
vim.opt.completeopt = {'menuone', 'noselect', 'noinsert'}
vim.opt.shortmess = vim.opt.shortmess + { c = true}
vim.api.nvim_set_option('updatetime', 300)

-- set only in md / org
vim.opt.conceallevel = 2
vim.opt.concealcursor = 'nc'

-- vim.opt.foldmethod = "expr"
-- vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

vim.cmd [[
    set nocompatible
    if has("autocmd")
      filetype plugin indent on
    endif
]]

-- vim.cmd [[
--     autocmd FileType markdown set foldexpr=NestedMarkdownFolds()
-- ]]

-- vim.cmd [[
--     au FileType org set noautoindent
-- ]]
