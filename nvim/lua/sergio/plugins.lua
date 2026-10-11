vim.pack.add({
  -- treesitter (main branch API: parsers installed via :lua and highlight via vim.treesitter.start)
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },

  -- LSP is fully native: per-server configs live in the top-level lsp/ dir,
  -- enabled from lua/sergio/lsp.lua. No nvim-lspconfig.

  -- fuzzy finder
  { src = "https://github.com/nvim-lua/plenary.nvim" },
  { src = "https://github.com/nvim-telescope/telescope.nvim" },
  -- compiled C sorter (needs `make`; built on install/update below) for speed
  { src = "https://github.com/nvim-telescope/telescope-fzf-native.nvim" },

  -- quick file marks
  { src = "https://github.com/ThePrimeagen/harpoon", version = "harpoon2" },

  -- file explorer as a buffer
  { src = "https://github.com/stevearc/oil.nvim" },

  -- git
  { src = "https://github.com/tpope/vim-fugitive" },

  -- colorschemes are installed in colors.lua
})

-- Build steps for plugins that ship native code (vim.pack has no build hook).
vim.api.nvim_create_autocmd("PackChanged", {
  group = vim.api.nvim_create_augroup("sergio/pack", { clear = true }),
  callback = function(args)
    local data = args.data or {}
    local name = data.spec and data.spec.name
    local kind = data.kind -- "install" | "update" | "delete"

    if name == "nvim-treesitter" and kind == "update" then
      vim.schedule(function()
        pcall(vim.cmd, "TSUpdate")
      end)
    end

    if name == "telescope-fzf-native.nvim" and (kind == "install" or kind == "update") then
      vim.notify("Building telescope-fzf-native...", vim.log.levels.INFO)
      vim.fn.system({ "make", "-C", data.path })
    end
  end,
})
