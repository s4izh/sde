{ pkgs }:
{
  appName = "nvim-mnw";
  neovim = pkgs.neovim-unwrapped;
  # initLua = ''
  #   require('myconfig')
  # '';
  extraBinPath = [
    pkgs.clang
    pkgs.gitlab-ci-ls
    pkgs.lua-language-server
    pkgs.bash-language-server
    pkgs.yaml-language-server
    pkgs.ruff
    pkgs.ty
    pkgs.rust-analyzer
    pkgs.tombi
  ];
  plugins = {
    start = with pkgs.vimplugins; [
      oil-nvim
      fzf-lua
      vim-tmux-navigator
      luasnip
      nvim-treesitter.withallgrammars
      vim-fugitive
      kanagawa-nvim
      nvim-web-devicons
      nvim-cmp
      cmp-buffer
      cmp-path
      cmp-nvim-lsp
      lualine-nvim
    ];
    dev.myconfig = {
      pure = ../../nvim;
      impure = "/home/sergio/personal/sde/nvim";
    };
  };
}

