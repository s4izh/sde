local fugitive = {
  "tpope/vim-fugitive",
  event = "VeryLazy",
  config = function()
    require("sergio.plugins.fugitive")
  end
}

local neogit = {
  "NeogitOrg/neogit",
  event = "VeryLazy",
}

return {
  fugitive,
  neogit,
}
