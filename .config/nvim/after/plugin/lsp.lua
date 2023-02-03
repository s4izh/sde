--  Language Server Protocol

require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = { "sumneko_lua", "rust_analyzer", "clangd", "pyright" }
})

-- LSP Diagnostics Options Setup
local sign = function(opts)
    vim.fn.sign_define(opts.name, {
        texthl = opts.name,
        text = opts.text,
        numhl = ''
    })
end

-- sign({ name = 'DiagnosticSignError', text = '' })
-- sign({ name = 'DiagnosticSignWarn', text = '' })
-- sign({ name = 'DiagnosticSignHint', text = '' })
-- sign({ name = 'DiagnosticSignInfo', text = '' })

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    update_in_insert = true,
    underline = true,
    severity_sort = false,
})

-- vim.cmd([[
-- set signcolumn=yes
-- autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
-- ]])

------- rust tools ------------

local rt = require("rust-tools")

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            -- vim.keymap.set("n", "<Leader>i", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- vim.keymap.set("n", "<Leader>i", {rt.hover_actions.hover_actions & vim.cmd("<C-w>w")}, { buffer = bufnr })
            -- Code action groups
            -- vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})

-- python
require 'lspconfig'.pyright.setup {
    on_attach = on_attach
}

-- c
require 'lspconfig'.clangd.setup {
    on_attach = on_attach
}

-- lua
require 'lspconfig'.sumneko_lua.setup {
    on_attach = on_attach
}

-- require'lspconfig'.jdtls.setup{
--     on_attach = on_attach
-- }

-- keybinds

vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function()
        local bufmap = function(mode, lhs, rhs)
            local opts = { buffer = true }
            vim.keymap.set(mode, lhs, rhs, opts)
        end

        bufmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>')
        bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')
        bufmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>')
        bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>')
        bufmap('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
        bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>')
        bufmap('n', '<Leader>i', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
        bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>')
        bufmap('n', '<Leader>a', '<cmd>lua vim.lsp.buf.code_action()<cr>')
        bufmap('n', 'g,', '<cmd>lua vim.diagnostic.goto_prev()<cr>')
        bufmap('n', 'g.', '<cmd>lua vim.diagnostic.goto_next()<cr>')
        bufmap('n', '<Leader>k', '<cmd>lua vim.diagnostic.open_float()<cr>')
        bufmap('n', '<Leader>F', '<cmd>lua vim.lsp.buf.format()<cr>')
    end
})

-- vim.cmd [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
