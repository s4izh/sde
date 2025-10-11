return {
  -- {
  --     "junegunn/fzf.vim",
  --     commit = '45d96c9',
  --     config = function()
  --         require("sergio.plugins.fzf")
  --     end,
  -- },
  -- {
  --     "junegunn/fzf",
  --     commit = '128e4a2',
  -- },
  "vijaymarupudi/nvim-fzf",
  {
    "ibhagwan/fzf-lua",
    enabled = true,
    -- optional for icon support
    -- dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("sergio.plugins.fzf").fzf_lua.setup()
    end
  }
}
