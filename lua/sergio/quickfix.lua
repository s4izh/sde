function ToggleQuickfix()
  local qf_exists = false
  for _, win in pairs(vim.fn.getwininfo()) do
    if win.quickfix == 1 then
      qf_exists = true
      break
    end
  end

  if qf_exists then
    vim.cmd('cclose')
  else
    vim.cmd('copen')
  end
end

vim.api.nvim_set_keymap('n', '<Leader>q', ':lua ToggleQuickfix()<CR>', { noremap = true, silent = true })
