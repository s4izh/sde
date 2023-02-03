-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- colorschemes
    use 'morhetz/gruvbox'
    use 'folke/tokyonight.nvim'
    use 'tanvirtin/monokai.nvim'
    use 'sainnhe/everforest'
    use 'dylanaraps/wal.vim'
    use 'Mofiqul/dracula.nvim'
    use 'navarasu/onedark.nvim'
    use({ 'rose-pine/neovim', as = 'rose-pine' })
    use 'shaunsingh/nord.nvim'
    use 'tomasiser/vim-code-dark'
    use 'svrana/neosolarized.nvim'
    use 'maxmx03/solarized.nvim'
    use 'squarefrog/tomorrow-night.vim'
    use 'wesgibbs/vim-irblack'

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    use({ 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' })

    use('nvim-treesitter/playground')
    use('theprimeagen/harpoon')
    use('mbbill/undotree')
    use('tpope/vim-fugitive')

    --lsp config
    use {
        { 'neovim/nvim-lspconfig' },
        { 'williamboman/mason.nvim' },
        { 'williamboman/mason-lspconfig.nvim' },
        -- Autocompletion
        { 'hrsh7th/nvim-cmp' },
        { 'hrsh7th/cmp-buffer' },
        { 'hrsh7th/cmp-path' },
        { 'saadparwaiz1/cmp_luasnip' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-nvim-lsp-signature-help' },
        { 'hrsh7th/cmp-nvim-lua' },
        -- Snippets
        { 'L3MON4D3/LuaSnip' },
        { 'rafamadriz/friendly-snippets' },
    }
    use 'simrat39/rust-tools.nvim'
    use 'onsails/lspkind.nvim'

    use { 'tpope/vim-commentary' }

    use {
        "windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }

    -- debugger
    use 'mfussenegger/nvim-dap'

    -- use("folke/zen-mode.nvim")
    -- use("github/copilot.vim")

    use 'vimwiki/vimwiki'

    -- use 'tools-life/taskwiki'

    -- markdown support
    use 'preservim/vim-markdown'
    use 'masukomi/vim-markdown-folding'

    -- zen mode goyo
    use 'junegunn/goyo.vim'
    use 'junegunn/limelight.vim'

    -- table mode
    use 'dhruvasagar/vim-table-mode'

    use {
        'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' },
        config = function() require('gitsigns').setup() end
    }

    use 'norcalli/nvim-colorizer.lua'

    use 'nvim-orgmode/orgmode'
    -- use { 'lukas-reineke/headlines.nvim' }
    -- use {'akinsho/org-bullets.nvim', config = function()
    --     require('org-bullets').setup()
    -- end}

    use { 'michaelb/sniprun' }

    -- use({
    --     "jackMort/ChatGPT.nvim",
    --     config = function()
    --         require("chatgpt").setup({
    --             -- optional configuration
    --         })
    --     end,
    --     requires = {
    --         "MunifTanjim/nui.nvim",
    --         "nvim-lua/plenary.nvim",
    --         "nvim-telescope/telescope.nvim"
    --     }
    -- })
end)
