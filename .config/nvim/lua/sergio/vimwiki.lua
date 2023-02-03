vim.cmd [[
    let g:vimwiki_list = [{'path':'~/vimwiki/notes', 'syntax':'markdown', 'ext': '.md'}]
]]

-- let g:vimwiki_list = [{'path':'~/vimwiki/notes', 'syntax':'markdown', 'ext': '.md'}]

vim.cmd [[
    let g:vimwiki_ext2syntax = {'.md':'markdown', '.markdown':'markdown', '.mdown': 'markdown'}
]]

-- makes links as [file](file.md) instead of [file](file)
vim.cmd [[
    let g:vimwiki_markdown_link_ext = 1
]]

-- vim.cmd [[
-- let g:taskwiki_markup_syntax = 'markdown'
-- ]]

vim.cmd [[
    let g:markdown_folding = 1
]]


vim.cmd [[
    let g:vimwiki_folding = 'syntax'
]]

-- vim.cmd.markdown_folding("1")


-- vim.cmd [[
--     let g:vim_markdown_folding_disabled = 0
-- ]]

vim.cmd[[
    let g:limelight_conceal_ctermfg=244
]]

vim.cmd[[
    let g:vim_markdown_fenced_languages = ['c++=cpp', 'viml=vim', 'bash=sh', 'ini=dosini']
]]

vim.cmd[[
    let g:vim_markdown_conceal_code_blocks = 0
]]

vim.cmd[[
    au BufRead,BufNewFile *.md set filetype=markdown
]]
