return {
    'dhruvasagar/vim-table-mode',
    'preservim/vim-markdown',
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    config = function()
        vim.g.mkdp_filetypes = { "markdown" }
    end,
    ft = { "markdown" }
    -- "vim-pandoc/vim-pandoc",
    -- "vim-pandoc/vim-pandoc-syntax",
    -- 'plasticboy/vim-markdown',
    -- 'lervag/wiki.vim',
}
