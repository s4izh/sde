if (vim.fn.executable("rg") > 0) then
  vim.o.grepprg = "rg --vimgrep --smart-case --hidden"
  vim.o.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end

local augroup = vim.api.nvim_create_augroup('GrepAutoQF', { clear = true })

vim.api.nvim_create_autocmd('QuickFixCmdPost', {
  group = augroup,
  pattern = 'grep',
  -- open the window at the bottom, spanning the full width
  command = 'botright cwindow'
})
