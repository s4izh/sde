-- base ref used by the diff mapping below
vim.g.git_base = vim.g.git_base or "HEAD"

vim.keymap.set("n", "<leader>gs", ":Ge:<CR>")
vim.keymap.set("n", "<leader>gd", ":set nosplitright<CR>:execute 'Gvdiff ' .. g:git_base<CR>:set splitright<CR>")
vim.keymap.set("n", "<leader>gb", ":Git blame<CR>")
vim.keymap.set("n", "<leader>gc", ":0Gclog<CR>", { noremap = false })

vim.api.nvim_create_autocmd("BufWinEnter", {
  group = vim.api.nvim_create_augroup("my_fugitive_group", {}),
  pattern = "*",
  callback = function()
    if vim.bo.ft ~= "fugitive" then
      return
    end

    local bufnr = vim.api.nvim_get_current_buf()
    local opts = { buffer = bufnr, remap = false }
    vim.keymap.set("n", "<leader>gp", function()
      vim.cmd.Git("push")
    end, opts)

    vim.keymap.set("n", "<leader>gP", function()
      vim.cmd.Git({ "pull" })
    end, opts)
  end,
})
