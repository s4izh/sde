local cmp = require("cmp")
local cmp_select = { behavior = cmp.SelectBehavior.Select }
cmp.setup({
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'pandoc' },
    { name = 'luasnip' },         -- For luasnip users.
    { name = 'buffer' },
    -- { name = 'crates' },
    -- {
    --   name = "ctags",
    --   -- default values
    --   option = {
    --     executable = "ctags",
    --     trigger_characters = { "." },
    --     trigger_characters_ft = {},
    --   },
    -- },
    -- {
      -- name = 'tags',
      -- option = {
        --   -- this is the default options, change them if you want.
        --   -- Delayed time after user input, in milliseconds.
        --   complete_defer = 100,
        --   -- Max items when searching `taglist`.
        --   max_items = 10,
        --   -- Use exact word match when searching `taglist`, for better searching
        --   -- performance.
        --   exact_match = false,
        --   -- Prioritize searching result for current buffer.
        --   current_buffer_only = false,
        -- },
        -- },
      }),
      mapping = cmp.mapping.preset.insert({
        ['<C-p>'] = function(fallback)
          if cmp.visible() then
            -- cmp.mapping.select_prev_item(cmp_select)
            cmp.select_prev_item()
          else
            cmp.complete()
            cmp.select_prev_item()
          end
        end,
        ['<C-n>'] = function(fallback)
          if cmp.visible() then
            -- cmp.mapping.select_next_item(cmp_select)
            cmp.select_next_item()
          else
            cmp.complete()
            cmp.select_next_item()
          end
        end,

        ['<C-y>'] = cmp.mapping.confirm({ select = true }),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
      }),
      snippet = {
        expand = function(args)
          require('luasnip').lsp_expand(args.body)           -- For `luasnip` users.
        end,
      },
      -- window = {
      --   completion = cmp.config.window.bordered(),
      --   documentation = cmp.config.window.bordered(),
      -- },
      formatting = {
        format = function(entry, vim_item)
          vim_item.abbr = string.sub(vim_item.abbr, 1, 30)
          return vim_item
        end
      },
      completion = {
        autocomplete = false,
      },
    })
    require("luasnip.loaders.from_vscode").lazy_load()

    local ls = require("luasnip")

    vim.keymap.set({ "i", "s" }, "<C-d>", function() ls.jump(1) end, { silent = true })
    vim.keymap.set({ "i", "s" }, "<C-s>", function() ls.jump(-1) end, { silent = true })

    -- vim.keymap.set({"i"}, "<C-K>", function() ls.expand() end, {silent = true})
    -- vim.keymap.set({"i", "s"}, "<C-J>", function() ls.jump(-1) end, {silent = true})

    -- vim.keymap.set({"i", "s"}, "<C-d>", function()
      --     if ls.choice_active() then
      --         ls.change_choice(1)
      --     end
      -- end, {silent = true})

      -- local capabilities = require('cmp-nvim-lsp').default_capabilities()
      -- -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
      -- require('lspconfig')['pyright'].setup {
        --     capabilities = capabilities
        -- }

