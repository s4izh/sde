M = {}

M.fzf_vim = {
    setup = function()
        vim.g.fzf_layout = { window = { width = 0.8, height = 0.9, yoffset = 0.9 } }

        vim.cmd [[let $FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS . ' --reverse --ansi']]

        --vim.keymap.set("n", "<C-p>", function()
        ---- fuzzy.files ""
        --end)
        ---- vim.keymap.set("n", "<Space>gf", fuzzy.git_files)
        --vim.keymap.set("n", "<Space>n", function()
        ---- fuzzy.files(vim.fn.expand "%:h")
        --end)
        ---- vim.keymap.set("n", "<Space>f", fuzzy.symbols)
        vim.keymap.set("n", "<Space>fp", ":GitFiles<CR>")
        vim.keymap.set("n", "<Space>ff", ":Files<CR>")
        vim.keymap.set("n", "<Space>fb", ":Buffers<CR>")
        vim.keymap.set("n", "<Space>fc", ":Commands<CR>")
        vim.keymap.set("n", "<Space>fh", ":Helptags<CR>")
        vim.keymap.set("n", "<Space>fg", ":RG<CR>")
        --vim.keymap.set("n", "<Space>m", ":Marks<CR>")

        -- vim.cmd [[ let g:fzf_preview_window = ['right:50%', 'ctrl-_'] ]]
        vim.cmd [[ let g:fzf_preview_window = ['down:70%:hidden', 'ctrl-/'] ]]
    end
}

M.fzf_lua = {
    setup = function()
        -- calling `setup` is optional for customization
        local fzf_lua = require("fzf-lua");
        fzf_lua.setup({
            files = {
                actions = { ["ctrl-q"] = { fn = require "fzf-lua".actions.file_sel_to_qf, prefix = "select-all" } }
            }
        })
        vim.keymap.set("n", "<Space>fp", function() fzf_lua.git_files() end)
        vim.keymap.set("n", "<Space>ff", function() fzf_lua.files() end)
        vim.keymap.set("n", "<Space>fb", function() fzf_lua.buffers() end)
        vim.keymap.set("n", "<Space>fc", function() fzf_lua.commands() end)
        vim.keymap.set("n", "<Space>fh", function() fzf_lua.help() end)
        vim.keymap.set("n", "<Space>fg", function() fzf_lua.live_grep() end)
    end
}

return M
