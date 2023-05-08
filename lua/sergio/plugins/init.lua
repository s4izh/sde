return {

  "tpope/vim-commentary",
  "junegunn/fzf.vim",
  "junegunn/fzf",
  "junegunn/goyo.vim",
  "vim-airline/vim-airline",
  -- "nvim-lualine/lualine.nvim",
  "jremmen/vim-ripgrep",
  "github/copilot.vim",
  "christoomey/vim-tmux-navigator",
  "TimUntersberger/neogit",
  -- "HiPhish/nvim-ts-rainbow2",
  "lewis6991/gitsigns.nvim",
  config = function()
    require('gitsigns').setup()
  end
  -- 'luochen1990/rainbow'

    -- use 'tanvirtin/monokai.nvim'
    -- use 'dylanaraps/wal.vim'
    -- use 'Mofiqul/dracula.nvim'
    -- use 'navarasu/onedark.nvim'
    -- use 
    -- use 'svrana/neosolarized.nvim'
    -- use 'maxmx03/solarized.nvim'
    -- use 'squarefrog/tomorrow-night.vim'
    -- use 'wesgibbs/vim-irblack'

}

