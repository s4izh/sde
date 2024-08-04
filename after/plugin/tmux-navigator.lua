vim.g.tmux_navigator_no_mappings = 1
local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<M-h>", ":<C-U>TmuxNavigateLeft<cr>", opts)
vim.api.nvim_set_keymap("n", "<M-j>", ":<C-U>TmuxNavigateDown<cr>", opts)
vim.api.nvim_set_keymap("n", "<M-k>", ":<C-U>TmuxNavigateUp<cr>", opts)
vim.api.nvim_set_keymap("n", "<M-l>", ":<C-U>TmuxNavigateRight<cr>", opts)
vim.api.nvim_set_keymap("n", "<M-\\>", ":<C-U>TmuxNavigatePrevious<cr>", opts)
