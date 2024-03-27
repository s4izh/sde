local neotree = {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    -- "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
    "MunifTanjim/nui.nvim",
  },
  keys = {
    { "<leader>E", "<cmd>Neotree toggle<cr>", desc = "NeoTree" },
  },
}

local chadtree = {
  'ms-jpq/chadtree',
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
  },
  build = ":CHADdeps",
  keys = {
    { "<leader>E", "<cmd>CHADopen<cr>", desc = "CHADopen" },
  },
}

return neotree
