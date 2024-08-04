-- local fuzzy = require "fuzzy"
-- vim.g.fzf_buffers_jump = true
vim.g.fzf_layout = { window = { width = 0.9, height = 0.9 } }
-- vim.cmd [[let $FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS . ' --reverse --ansi']]

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
--vim.keymap.set("n", "<Space>m", ":Marks<CR>")
