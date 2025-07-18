local treesitter = require("nvim-treesitter.configs")

treesitter.setup({
    ensure_installed = { "c", "cpp", "lua", "rust", "vimdoc" }, -- vimdoc doesn't work on my nix yet
    sync_install = false,
    auto_install = true,
    -- highlight = false,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = true,
    },
    indent = {
        enable = true,
    },
    -- rainbow = {
    --     enable = true,
    -- }
})
