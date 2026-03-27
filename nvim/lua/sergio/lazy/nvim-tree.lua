return {
  "nvim-tree/nvim-tree.lua",
  dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
  config = function()
    vim.g.nvim_tree_show_icons = {
      git = 0,
      folders = 0,
      files = 0,
      folder_arrows = 0,
    }
    local config = {
      sort = {
        sorter = "case_sensitive",
      },
      view = {
        width = 40,
        side = "right",
      },
      renderer = {
        group_empty = true,
      },
      filters = {
        dotfiles = false,
      },
    }
    require("nvim-tree").setup(config)
    vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<cr>")
  end
}
