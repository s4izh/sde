local M = {}

M.opts = {
  colorcolumn = "100"
}

M.toggle_line_numbers = function()
  local is_number = vim.wo.number
  local is_relativenumber = vim.wo.relativenumber

  if is_number or is_relativenumber then
    vim.wo.number = false
    vim.wo.relativenumber = false
    print("Line numbers disabled")
  else
    vim.wo.number = true
    vim.wo.relativenumber = true
    print("Line numbers enabled")
  end
end

M.toggle_color_column = function()
  local col = vim.opt.colorcolumn._value
  if col == "0" then
    vim.opt.colorcolumn = M.opts.colorcolumn
  else
    vim.opt.colorcolumn = "0"
  end
end

return M
