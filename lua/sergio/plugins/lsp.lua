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

local lsp_minimal = {
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
    lsp.clangd.setup({
      capabilities = capabilities,
    })
    lsp.pyright.setup({})
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

        -- vim.api.nvim_create_autocmd('BufWritePre', {
        --   pattern = "*.lua",
        --   callback = function()
        --     vim.lsp.buf.format { async = true }
        --   end,
        -- })
      end,
    })
    -- adding borders to lsp
    local _border = "rounded"
    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
      vim.lsp.handlers.hover, {
        border = _border
      }
    )
    vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
      vim.lsp.handlers.signature_help, {
        border = _border
      }
    )
    vim.diagnostic.config {
      float = { border = _border }
    }
    require('lspconfig.ui.windows').default_options = {
      border = _border
    }
  end,
}

local lspzero = {
  'VonHeikemen/lsp-zero.nvim',
  branch = 'v1.x',
  dependencies = {
    -- LSP Support
    { 'neovim/nvim-lspconfig' },             -- Required
    { 'williamboman/mason.nvim' },           -- Optional
    { 'williamboman/mason-lspconfig.nvim' }, -- Optional

    -- Autocompletion
    { 'hrsh7th/nvim-cmp' },         -- Required
    { 'hrsh7th/cmp-nvim-lsp' },     -- Required
    { 'hrsh7th/cmp-buffer' },       -- Optional
    { 'hrsh7th/cmp-path' },         -- Optional
    { 'saadparwaiz1/cmp_luasnip' }, -- Optional
    { 'hrsh7th/cmp-nvim-lua' },     -- Optional
    { 'aspeddro/cmp-pandoc.nvim' },

    -- Snippets
    { 'L3MON4D3/LuaSnip' },             -- Required
    { 'rafamadriz/friendly-snippets' }, -- Optional
  },
  config = function()
    local lsp = require("lsp-zero")
    lsp.preset("recommended")
    lsp.ensure_installed({
      -- 'tsserver',
      'rust_analyzer',
      'lua_ls',
      -- 'clangd',
    })
    -- Fix Undefined global 'vim'
    lsp.configure('lua-language-server', {
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' }
          }
        }
      }
    })
    lsp.configure('gopls', {
      settings = {
        gopls = {
          completeUnimported = true,
          usePlaceholders = true,
          analyses = {
            unusedparams = true,
          },
          staticcheck = true,
        },
      },
    })
    local cmp = require('cmp')
    local cmp_select = { behavior = cmp.SelectBehavior.Select }
    local cmp_mappings = lsp.defaults.cmp_mappings({
      ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
      ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
      ['<C-y>'] = cmp.mapping.confirm({ select = true }),
      ["<C-Space>"] = cmp.mapping.complete(),
    })
    cmp_mappings['<Tab>'] = nil
    cmp_mappings['<S-Tab>'] = nil
    cmp_mappings['<return>'] = nil
    lsp.setup_nvim_cmp({
      mapping = cmp_mappings
    })
    lsp.set_preferences({
      suggest_lsp_servers = false,
      sign_icons = {
        error = 'E',
        warn = 'W',
        hint = 'H',
        info = 'I'
      }
    })
    lsp.nvim_workspace()

    lsp.on_attach(function(client, bufnr)
      local opts = { buffer = bufnr, remap = false }
      vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
      vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
      vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
      vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
      vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
      vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
      vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
      vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
      vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
      vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format)
      vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
      vim.keymap.set('n', '<leader>le', vim.diagnostic.open_float)
      -- if client.supports_method('textDocument/formatting') then
      --     vim.api.nvim_clear_autocmds({
      --         group = augroup,
      --         buffer = bufnr,
      --     })
      --     vim.api.nvim_create_autocmd("BufWritePre", {
      --         group = augroup,
      --         buffer = bufnr,
      --         callback = function()
      --             vim.lsp.buf.format({ bufnr = bufnr })
      --         end,
      --     })
    end)
    lsp.setup()
    vim.diagnostic.config({
      virtual_text = true
    })

    -- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
    -- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
    -- vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

    lsp.setup_nvim_cmp({
      sources = {
        -- {name = 'path'},
        -- {name = 'nvim_lsp'},
        -- {name = 'buffer', keyword_length = 3},
        -- {name = 'luasnip', keyword_length = 2},
        -- {name = 'cmp_pandoc', keyword_length = 2},
      }
    })
  end,
  -- 'simrat39/rust-tools.nvim',
}

return lsp_minimal

-- return lspzero
