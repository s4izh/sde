require("sergio.keymaps").group("<leader>g", "+git")

-- base ref used by the diff mapping below
vim.g.git_base = vim.g.git_base or "HEAD"

vim.keymap.set("n", "<leader>gs", ":Ge:<CR>", { desc = "Git status (fugitive)" })
vim.keymap.set("n", "<leader>gd", ":set nosplitright<CR>:execute 'Gvdiff ' .. g:git_base<CR>:set splitright<CR>", { desc = "Git diff file vs g:git_base" })
vim.keymap.set("n", "<leader>gb", ":Git blame<CR>", { desc = "Git blame" })
vim.keymap.set("n", "<leader>gc", ":0Gclog<CR>", { noremap = false, desc = "Git log of current file" })

vim.api.nvim_create_autocmd("BufWinEnter", {
  group = vim.api.nvim_create_augroup("my_fugitive_group", {}),
  pattern = "*",
  callback = function()
    if vim.bo.ft ~= "fugitive" then
      return
    end

    local bufnr = vim.api.nvim_get_current_buf()
    vim.keymap.set("n", "<leader>gp", function()
      vim.cmd.Git("push")
    end, { buffer = bufnr, remap = false, desc = "Git push" })

    vim.keymap.set("n", "<leader>gP", function()
      vim.cmd.Git({ "pull" })
    end, { buffer = bufnr, remap = false, desc = "Git pull" })
  end,
})
