vim.opt.termguicolors = true

function ColorMyPencils(color)
  color = color or "rose-pine"
  vim.cmd.colorscheme(color)
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

  vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "white", bg = "none" })
  vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
  -- vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none" })

  vim.api.nvim_set_hl(0, "LineNr", { bg = "none", ctermbg = "none", fg="grey" })

  vim.cmd [[
      highlight LineNr ctermbg=none guibg=none
  ]]

  -- vim.api.nvim_set_hl(0, "StatusLine", { bg = "black" })  -- Transparent status line
  vim.api.nvim_set_hl(0, "StatusLine", { fg = "white" })  -- Transparent status line
  vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none", fg = "grey" })  -- Inactive status line color

  -- Set the normal text color for inactive windows
  -- vim.api.nvim_set_hl(0, "NormalNC", { bg = "none", fg = "grey" })  -- Change 'grey' to your desired color
  vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })  -- Change 'grey' to your desired color

  vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none", fg = "grey" })  -- Use 'grey' or your preferred color
end

vim.opt.background = "dark"

-- require("rose-pine")

vim.g.gruvbox_contrast_dark = 'hard' -- Set desired contrast here

ColorMyPencils("gruber-darker")
