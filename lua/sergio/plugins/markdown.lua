local notes = {
  'lervag/wiki.vim',
  config = function ()
    -- local wiki = require('wiki')
    vim.g.wiki_root = '~/notes'
  end
}

local vim_markdown = {
  'preservim/vim-markdown',
  dependencies = {
    'godlygeek/tabular',
    -- 'masukomi/vim-markdown-folding'
  },
  config = function ()
    vim.g.vim_markdown_folding_disabled = 1
    vim.g.vim_markdown_conceal = 0
    vim.g.vim_markdown_conceal_code_blocks = 0
    vim.g.vim_markdown_conceal_code_blocks = 0
    vim.g.vim_markdown_frontmatter = 1
  end
}

return {
    'dhruvasagar/vim-table-mode',
    -- 'vim-scripts/DrawIt',
    -- "vim-pandoc/vim-pandoc",
    -- "vim-pandoc/vim-pandoc-syntax",
    -- 'plasticboy/vim-markdown',
    vim_markdown,
    notes,
}
