local function setup_theme(name, event, config)
  return {
    name,
    event = "VeryLazy",
    config = config,
  }
end

return {
  setup_theme("rose-pine/neovim", function()
    local rose_pine = require('rose-pine')
    -- rose_pine.setup({
    --   disable_italics = true
    -- })
  end),
  setup_theme("savq/melange-nvim"),
  setup_theme("thimc/gruber-darker.nvim"),
  setup_theme("eemed/sitruuna.vim"),
  setup_theme("morhetz/gruvbox"),
  setup_theme("RMichelsen/cyanide"),
  setup_theme("tjdevries/colorbuddy.vim"),
  setup_theme("tjdevries/gruvbuddy.nvim"),
  setup_theme("sainnhe/everforest"),
  setup_theme("ishan9299/modus-theme-vim"),
  setup_theme("gavinok/spaceway.vim"),
  setup_theme("xero/sourcerer.vim"),
  setup_theme("xero/vim-noctu"),
  setup_theme("maxmx03/solarized.nvim"),
  setup_theme("navarasu/onedark.nvim"),
}
