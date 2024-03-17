return {
    'nvim-telescope/telescope.nvim',
    -- tag = '0.1.1',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
        -- defaults = {
        --   file_ignore_patterns = { "^./img/", "^img/", "^vendor/" },
        -- }
        local builtin = require('telescope.builtin')
        vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
        -- vim.keymap.set('n', '<leader>ff', builtin.find_files(require('telescope.themes').get_dropdown, {}))
        -- vim.cmd [[
        --     -- nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files(require('telescope.themes').get_cursor({}))<cr>
        -- ]]
        vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
        -- vim.keymap.set('n', '<C-p>', builtin.git_files, {})
        vim.keymap.set('n', '<leader>fp', builtin.git_files, {})
        vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
        vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
        vim.keymap.set('n', '<leader>fs', builtin.grep_string, {})
        vim.keymap.set('n', '<leader>fS', function()
            builtin.grep_string({ search = vim.fn.input("Grep > ")});
        end)
    end
}
