-- Define your colors
local colors = {
  fg = "#CDCDCD",
  bg = "#000000",
  blue0 = "#7D8FA3",
  region = "#8fafd7",
  green0 = "#a1bf78",
  green1 = "#a1bf78",
  green2 = "#85A7A5",
  green3 = "#75b5aa",
  grey0 = "#888888",
  grey1 = "#7f7f7f",
  grey2 = "#151515",
  grey3 = "#1c1c1c",
  grey4 = "#363636",
  grey5 = "#444444",
  orange0 = "#E5C078",
  orange1 = "#f4bf75",
  purple0 = "#8197bf",
  purple1 = "#474e90",
  purple2 = "#9F7AA5",
  purple3 = "#aa759f",
  red0 = "#CC6666",
  red1 = "#af5f5f",
  red2 = "#d75f5f",
  red3 = "#ff73fd",
  yellow0 = "#E5C078",
  yellow1 = "#FFAF00",
  hl_line = "#14171B",
}

-- Define your theme
local theme = {
  Normal = { fg = colors.fg, bg = colors.bg },
  Comment = { fg = colors.grey1, bg = colors.bg, italic = true },
  Visual = { bg = colors.region },
  CursorLine = { bg = colors.hl_line },
  CursorLineNr = { fg = colors.blue1, bg = colors.hl_line },
  LineNr = { fg = colors.grey1, bg = colors.bg },
  StatusLine = { fg = colors.grey, bg = colors.grey3 },
  StatusLineNC = { fg = colors.grey1, bg = colors.grey2 },
  VertSplit = { fg = colors.grey4, bg = colors.bg },
  Pmenu = { fg = colors.fg, bg = colors.grey3 },
  PmenuSel = { fg = colors.bg, bg = colors.blue0 },
  PmenuSbar = { bg = colors.grey4 },
  PmenuThumb = { bg = colors.grey5 },
  Search = { fg = colors.bg, bg = colors.orange0 },
  IncSearch = { fg = colors.bg, bg = colors.orange1 },
  -- Add more highlight groups as needed
  -- Syntax highlighting groups
  String = { fg = colors.green1 },
  Function = { fg = colors.blue1 },
  Keyword = { fg = colors.blue0, bold = false },
  Identifier = { fg = colors.fg },
  Type = { fg = colors.yellow0 },
  StorageClass = { fg = colors.yellow0 }, -- To cover 'void' and similar keywords
  Constant = { fg = colors.red0 },
  -- More syntax groups can be added as needed
}

-- Apply the theme
for group, color in pairs(theme) do
  vim.api.nvim_set_hl(0, group, color)
end
