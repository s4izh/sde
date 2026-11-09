local M = {}

local config = require("sergio.config")

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

-- on: the column from config.lua; off: "" (also treats "0" as off)
M.toggle_color_column = function()
  local col = vim.o.colorcolumn
  if col == "" or col == "0" then
    vim.o.colorcolumn = tostring(config.colorcolumn.column)
  else
    vim.o.colorcolumn = ""
  end
end

return M
