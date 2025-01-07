return {
  "preservim/tagbar",
  config = function()
    vim.keymap.set("n", "<leader>tt", ":TagbarToggle<CR>")
  end
}
