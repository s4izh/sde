-- nvim-cmp: popup completion engine, an alternative to the native completion.
--
-- Sourcing this module (uncomment its require in init.lua) switches completion
-- from native to cmp: it sets vim.g.completion_engine = "cmp", which lsp.lua
-- checks to skip enabling native LSP completion. Comment the require out again
-- to go back to native completion.
vim.g.completion_engine = "cmp"

vim.pack.add({
  { src = "https://github.com/hrsh7th/nvim-cmp" },
  { src = "https://github.com/hrsh7th/cmp-nvim-lsp" },
  { src = "https://github.com/hrsh7th/cmp-buffer" },
  { src = "https://github.com/hrsh7th/cmp-path" },
})

local cmp = require("cmp")

cmp.setup({
  -- no popup while typing: the menu only appears when asked for, via <C-n>
  -- (or <C-Space>)
  completion = {
    autocomplete = false,
  },

  -- the documentation window stays shut unless asked for (<C-d> toggles it)
  view = {
    docs = { auto_open = false },
  },

  -- rounded borders, matching the LSP hover/signature floats in lsp.lua
  window = {
    completion = cmp.config.window.bordered({ border = "rounded" }),
    documentation = cmp.config.window.bordered({ border = "rounded" }),
  },
  snippet = {
    -- native snippets (Nvim 0.10+), so no luasnip dependency
    expand = function(args)
      vim.snippet.expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-Space>"] = cmp.mapping.complete(),

    -- show the docs for the selected entry on demand (auto_open is off above)
    ["<C-d>"] = cmp.mapping(function()
      if cmp.visible_docs() then
        cmp.close_docs()
      else
        cmp.open_docs()
      end
    end, { "i", "s" }),
    ["<C-e>"]     = cmp.mapping.abort(),
    ["<CR>"]      = cmp.mapping.confirm({ select = false }),
    -- <C-n> opens the menu (completion is manual, see completion.autocomplete
    -- below) and then walks down it; <C-p> walks back up.
    ["<C-n>"] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_next_item()
      else
        cmp.complete()
      end
    end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      else
        cmp.complete()
      end
    end, { "i", "s" }),

    -- <Tab> is overloaded: pick a completion, else jump to the next snippet
    -- placeholder (the call arguments inserted on confirm), else a literal Tab.
    -- Without the middle case cmp's mapping would shadow Nvim's default
    -- <Tab> = vim.snippet.jump and argument placeholders would be unreachable.
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif vim.snippet.active({ direction = 1 }) then
        vim.snippet.jump(1)
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif vim.snippet.active({ direction = -1 }) then
        vim.snippet.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "path" },
  }, {
    { name = "buffer" },
  }),
})

-- Advertise cmp's richer completion capabilities to every LSP server. The native
-- lsp/<name>.lua configs inherit this via the "*" wildcard config.
vim.lsp.config("*", {
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
})
