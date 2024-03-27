return {
    "ibhagwan/fzf-lua",
    -- optional for icon support
    -- dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        -- calling `setup` is optional for customization
        require("fzf-lua").setup({})
    end,
    -- keys = {
    --     -- { "<leader>ff", "<cmd>FzfLua files<cr>" },
    --     -- { "<leader>fg", "<cmd>FzfLua grep_visual<cr>" },
    --     -- { "<C-p>", "<cmd>FzfLua git_files<cr>" },
    -- },
}
