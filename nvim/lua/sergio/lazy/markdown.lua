local render_markdown = {
  'MeanderingProgrammer/render-markdown.nvim',
  enabled = true,
  -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
  -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
  dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
  -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
  ---@module 'render-markdown'
  opts = {
    heading = {
      enabled = false,
      sign = false,
      width = 'block',
    },
    code = {
      enabled = true,
      sign = false,
      style = 'normal',
    },
    dash = {
      enabled = false,
    },
    -- indent = {
    --   enabled = true,
    -- },
  },
}

local markdown_toc = {
  "mzlogin/vim-markdown-toc",
}

local vim_table_mode = {
  "dhruvasagar/vim-table-mode",
  ft = { "markdown" },
  config = function()
    vim.keymap.set("n", "<Leader>tm", ":TableModeToggle<CR>", { noremap = true, silent = true })
    vim.keymap.set("n", "<Leader>tr", ":TableModeRealign<CR>", { noremap = true, silent = true })
  end,
}

local markdown_preview = {
  "iamcco/markdown-preview.nvim",
  build = "cd app && yarn install",
  init = function()
    vim.g.mkdp_filetypes = { "markdown" }
  end,
  ft = { "markdown" },
}

return {
  -- render_markdown,
  markdown_toc,
  vim_table_mode,
  markdown_preview,
}
