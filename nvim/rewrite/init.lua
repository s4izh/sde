vim.g.mapleader   = " "
vim.opt.guicursor = ""

require("sergio")

if (vim.fn.executable("rg") > 0) then
  vim.api.nvim_set_option("grepprg", "rg --vimgrep --smart-case --hidden")
  vim.api.nvim_set_option("grepformat", "%f:%l:%c:%m,%f:%l:%m")
end
