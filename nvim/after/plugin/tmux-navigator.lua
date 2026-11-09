vim.g.tmux_navigator_no_mappings = 1
local function opts(desc) return { noremap = true, silent = true, desc = desc } end
vim.api.nvim_set_keymap("n", "<M-h>", ":<C-U>TmuxNavigateLeft<cr>", opts("Go to left split / tmux pane"))
vim.api.nvim_set_keymap("n", "<M-j>", ":<C-U>TmuxNavigateDown<cr>", opts("Go to lower split / tmux pane"))
vim.api.nvim_set_keymap("n", "<M-k>", ":<C-U>TmuxNavigateUp<cr>", opts("Go to upper split / tmux pane"))
vim.api.nvim_set_keymap("n", "<M-l>", ":<C-U>TmuxNavigateRight<cr>", opts("Go to right split / tmux pane"))
vim.api.nvim_set_keymap("n", "<M-\\>", ":<C-U>TmuxNavigatePrevious<cr>", opts("Go to previous split / tmux pane"))
