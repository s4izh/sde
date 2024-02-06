return {
  "tpope/vim-commentary",
  "tpope/vim-dispatch",
  "tpope/vim-surround",
  "junegunn/goyo.vim",
  -- "vim-airline/vim-airline",
  "halkn/ripgrep.vim",
  "editorconfig/editorconfig-vim",
  "kmonad/kmonad-vim", -- support for kmonad files
  {
    "xiyaowong/transparent.nvim",
    config = function()
      require("transparent").setup()
      vim.cmd([[TransparentEnable]])
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("gitsigns").setup()
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      vim.opt.termguicolors = true
      require("colorizer").setup()
    end,
  },
  {
    "oncomouse/lushwal.nvim",
    cmd = { "LushwalCompile" },
    dependencies = {
      { "rktjmp/lush.nvim" },
      { "rktjmp/shipwright.nvim" },
    },
  },
  "RaafatTurki/hex.nvim", -- requieres xdd
  -- "ctrlpvim/ctrlp.vim",
  -- "chrisbra/vim-zsh",
  -- "mhinz/vim-startify",
  -- "startup-nvim/startup.nvim",
  -- "HiPhish/nvim-ts-rainbow2",
  -- 'luochen1990/rainbow'
  -- "lukas-reineke/indent-blankline.nvim",
}
