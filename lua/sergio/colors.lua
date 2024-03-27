vim.opt.termguicolors = true

function ColorMyPencils(color)
  color = color or "rose-pine"
  vim.cmd.colorscheme(color)
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
  vim.cmd [[
        highlight LineNr ctermbg=none guibg=none
    ]]
end

vim.opt.background = "dark"

-- ColorMyPencils("zenburn")
require('colorbuddy').colorscheme('gruvbuddy')
