return {
    "mbbill/undotree",
    -- keys = {
    --     { "<leader>u", vim.cmdUndoTreeToggle , desc = "NeoTree" },
    -- },
    config = function()
        vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
    end
}
