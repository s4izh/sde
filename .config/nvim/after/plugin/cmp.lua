local lspkind = require "lspkind"
lspkind.init()

local cmp = require 'cmp'
cmp.setup({
    -- read :help ins-completion
    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-e>'] = cmp.mapping.close(),
        ['<C-y>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        }),
        ['<C-Space>'] = cmp.mapping.complete(),
    },
    -- Installed sources:
    sources = {
        { name = 'path' }, -- file paths
        { name = 'nvim_lsp', keyword_length = 3 }, -- from language server
        { name = 'nvim_lsp_signature_help' }, -- display function signatures with current parameter emphasized
        { name = 'nvim_lua', keyword_length = 2 }, -- complete neovim's Lua runtime API such vim.lsp.*
        { name = 'buffer', keyword_length = 3 }, -- source current buffer
        -- { name = 'vsnip', keyword_length = 2 }, -- nvim-cmp source for vim-vsnip
        { name = 'luasnip', keyword_length = 2 }, -- nvim-cmp source for vim-vsnip
        { name = 'calc' }, -- source for math calculation
        { name = 'orgmode' }
    },
    -- window = {
    --     completion = cmp.config.window.bordered(),
    --     documentation = cmp.config.window.bordered(),
    -- },
    -- Enable LSP snippets
    snippet = {
        -- expand = function(args)
        --     vim.fn["vsnip#anonymous"](args.body)
        -- end,
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },

    formatting = {
        -- fields = {'menu', 'abbr', 'kind'},
        -- format = function(entry, item)
        --     -- local menu_icon ={
        --     --     nvim_lsp = 'λ',
        --     --     vsnip = '⋗',
        --     --     buffer = 'Ω',
        --     --     path = '🖫',
        --     -- }
        --     local menu_icon ={
        --         nvim_lsp = '[LSP]',
        --         vsnip = '[vsnip]',
        --         buffer = '[buf]',
        --         path = '[path]',
        --     }
        --     item.menu = menu_icon[entry.source.name]
        --     return item
        -- end,
        format = lspkind.cmp_format {
            with_text = true;
            menu = {
                buffer = "[buff]",
                nvim_lsp = "[LSP]",
                nvim_lua = "[api]",
                path = "[path]",
                luasnip = "[snip]",
                -- gh_issues = "[issues]",
            }
        }
    },
    experimental = {
        native_menu = false,
        ghost_text = true;
    },
})
