return {
    "github/copilot.vim",
    config = function()
        vim.cmd [[
            imap <silent><script><expr> <C-J> copilot#Accept("\<CR>")
            let g:copilot_no_tab_map = v:true
        ]]
        -- vim.api.nvim_set_keymap('i', '<C-J>', '<Cmd>lua require("copilot").accept()<CR>', { noremap = true, silent = true, expr = true })
        -- vim.g.copilot_no_tab_map = true
    end,
}
