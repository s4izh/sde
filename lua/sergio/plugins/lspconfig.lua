local function show_documentation()
  local filetype = vim.bo.filetype
  if vim.tbl_contains({ 'vim', 'help' }, filetype) then
    vim.cmd('h ' .. vim.fn.expand('<cword>'))
  elseif vim.tbl_contains({ 'man' }, filetype) then
    vim.cmd('Man ' .. vim.fn.expand('<cword>'))
  elseif vim.fn.expand('%:t') == 'Cargo.toml' and require('crates').popup_available() then
    require('crates').show_popup()
  end
end


local lspconfig = {
  "neovim/nvim-lspconfig",
  config = function()
    local lsp = require("lspconfig")
    local capabilities = require("cmp_nvim_lsp").default_capabilities()
    local format_group = vim.api.nvim_create_augroup('FormatGroup', {})

    lsp.rust_analyzer.setup({
      capabilities = capabilities,
    })

    lsp.gopls.setup({})
    lsp.tsserver.setup({})
    lsp.bashls.setup({})
    lsp.zls.setup({})
    lsp.pyright.setup({})

    lsp.clangd.setup({
      cmd = { "clangd",  "-cross-file-rename" },
      capabilities = capabilities,
    })

    lsp.lua_ls.setup({
      cmd = { "lua-language-server" },
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
        },
      },
    })

    lsp.ltex.setup({
      on_attach = on_attach,
      capabilities = capabilities,
      cmd = { "ltex-ls" },
      filetypes = { "tex", "bib" },
      settings = {
        ltex = {
          language = "es",
        },
      },
    })

    lsp.texlab.setup({
      on_attach = on_attach,
      capabilities = capabilities,
      cmd = { "texlab" },
      filetypes = { "tex", "bib" },
    })

    lsp.nixd.setup({
      on_attach = on_attach,
      capabilities = capabilities,
      cmd = { "nixd" },
      filetypes = { "nix" },
    })

    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = { buffer = ev.buf }
        vim.keymap.set("n", "<Leader>vla", ":LspStop<cr>")
        vim.keymap.set("n", "<Leader>vlr", ":LspRestart<cr>")
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
        vim.keymap.set('n', '<leader>lq', vim.diagnostic.setloclist)
        vim.keymap.set('n', '<leader>le', vim.diagnostic.open_float)
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        -- vim.keymap.set('n', 'gD', vim.lsp.buf.type_definition, opts)
        if client.server_capabilities.hoverProvider then
          vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        else
          vim.keymap.set('n', 'K', show_documentation, { silent = true })
        end
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<leader>vls', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', 'gs', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set("n", "<leader>lws", function() vim.lsp.buf.workspace_symbol() end, opts)
        vim.keymap.set('n', '<leader>lwl', function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, opts)
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<space>vrn', vim.lsp.buf.rename, opts)
        vim.keymap.set({ 'n', 'v' }, '<space>vca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<space>lf', function()
          vim.lsp.buf.format { async = true }
        end, opts)

        vim.keymap.set('n', '<space>ls', ":LspStop<cr>", opts)

        vim.api.nvim_set_hl(0, 'FloatBorder', { link = 'Normal'})

        -- vim.api.nvim_create_autocmd('BufWritePre', {
        --   pattern = "*.lua",
        --   callback = function()
        --     vim.lsp.buf.format { async = true }
        --   end,
        -- })
      end,
    })

    -- adding borders to lsp
    -- local _border = "rounded"
    -- vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    --   vim.lsp.handlers.hover, {
    --     border = _border
    --   })

    -- vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
    --   vim.lsp.handlers.signature_help, {
    --     border = _border
    --   })

    -- vim.diagnostic.config ({
    --   float = { border = _border }
    -- })

    -- require('lspconfig.ui.windows').default_options = {
    --   border = _border
    -- }


  end,
}

return lspconfig
