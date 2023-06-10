return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  config = function()
    require'nvim-treesitter.configs'.setup {
      ensure_installed = { "javascript", "c", "lua", "rust" }, -- vimdoc doesn't work on my nix yet
      sync_install = false,
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      rainbow = {
        enable = true,
      }
    }
  end
}
