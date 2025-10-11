return {
  "folke/snacks.nvim",
  event = "VeryLazy",
  config = function()
    local feature_toggler = require("sergio.plugins.snacks")

    require("snacks").setup({
      image = {
        -- enabled = feature_toggler.image_previews_enabled,
        enabled = false,
      },
    })
  end,
}
