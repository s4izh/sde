return {
  "nvim-tree/nvim-tree.lua",
  config = function()
    vim.g.nvim_tree_show_icons = {
      git = 0,
      folders = 0,
      files = 0,
      folder_arrows = 0,
    }
    require("nvim-tree").setup({})
    vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<cr>")
  end
}
