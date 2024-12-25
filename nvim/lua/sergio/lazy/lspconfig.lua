return {
  "neovim/nvim-lspconfig",
  dependencies = "netmute/ctags-lsp.nvim",
  config = function()
    require("sergio.plugins.lspconfig")
  end,
}
