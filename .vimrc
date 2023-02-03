set relativenumber
set number

set nocompatible

syntax on

let mapleader = " "

set mouse="a"

set number

set encoding=utf-8

set wrap
set textwidth=79
set formatoptions=tcqrn1

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set noshiftround

set scrolloff=8
set backspace=indent,eol,start
set matchpairs+=<:> " use % to jump between pairs

" Move up/down editor lines
nnoremap j gj
nnoremap k gk

" Allow hidden buffers
set hidden

" Rendering
set ttyfast

set hlsearch
set incsearch
set ignorecase
set smartcase

" set showmatch
map <leader><space> :let @/=''<cr> " clear search

" Formatting
map <leader>q gqip
