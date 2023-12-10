local tmux_navigator = {
  "christoomey/vim-tmux-navigator",
  config = function()
    vim.g.tmux_navigator_no_mappings = 1
    local opts = { noremap = true, silent = true }
    vim.api.nvim_set_keymap("n", "<M-h>", ":<C-U>TmuxNavigateLeft<cr>", opts)
    vim.api.nvim_set_keymap("n", "<M-j>", ":<C-U>TmuxNavigateDown<cr>", opts)
    vim.api.nvim_set_keymap("n", "<M-k>", ":<C-U>TmuxNavigateUp<cr>", opts)
    vim.api.nvim_set_keymap("n", "<M-l>", ":<C-U>TmuxNavigateRight<cr>", opts)
    vim.api.nvim_set_keymap("n", "<M-\\>", ":<C-U>TmuxNavigatePrevious<cr>", opts)
  end,
}

local slime = {
  "jpalardy/vim-slime",
  config = function()
    vim.g.slime_target = "tmux"
    vim.g.slime_paste_file = vim.fn.tempname()
    -- vim.api.nvim_set_var('slime_paste_file', vim.fn.tempname())
    -- vim.cmd('let g:slime_paste_file = tempname()')
  end,
}

return {
  tmux_navigator,
  slime,
}
