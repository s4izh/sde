set encoding=utf-8
set relativenumber
set number
set mouse="a"
set nocompatible
set noswapfile

syntax on

set path+=**

let mapleader = " "

set wrap
set textwidth=79
set formatoptions=tcqrn1

set completeopt=menu,menuone,noselect,noinsert

set autoindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set noshiftround

set scrolloff=8
set backspace=indent,eol,start

set matchpairs+=<:> " use % to jump between pairs

set ttyfast

" Move up/down editor lines
nnoremap j gj
nnoremap k gk

" Scroll and center cursor
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" Allow hidden buffers
set hidden

""" Search
set hlsearch
set incsearch
set ignorecase
set smartcase

""" Keybindings

" set showmatch
" map <leader>n :let @/=''<cr> " clear search
map <leader>n :noh<cr>

" Formatting
map <leader>q gqip
