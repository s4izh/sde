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

local gitlinker = {
  "linrongbin16/gitlinker.nvim",
  cmd = "GitLink",
  opts = {},
  keys = {
    { "<leader>gy", "<cmd>GitLink<cr>",  mode = { "n", "v" }, desc = "Yank git link" },
    { "<leader>gY", "<cmd>GitLink!<cr>", mode = { "n", "v" }, desc = "Open git link" },
  },
}

return {
  fugitive,
  neogit,
  gitlinker,
}
