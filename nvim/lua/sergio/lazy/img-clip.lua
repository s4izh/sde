-- lua/plugins/img-clip.lua

return {
  "HakonHarnes/img-clip.nvim",
  event = "VeryLazy",
  -- The `config` function runs when the plugin is loaded.
  -- We use it to call our manager's setup function.
  config = function()
    require("sergio.plugins.img-clip").setup()
  end,
  keys = {
    { "<leader>p", "<cmd>PasteImage<cr>", desc = "Paste image from clipboard" },
  },
}
