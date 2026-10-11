-- Spaceway colorscheme for Neovim (structure based on ziggy.lua)
-- Inspired by the Emacs spaceway theme by Mark Tran

if vim.fn.has('nvim') == 1 and vim.fn.has('nvim-0.9') == 0 then
    vim.api.nvim_echo({
        { "spaceway requires Neovim 0.9 or later.\n", "WarningMsg" }
    }, false, {})
    return
end

vim.cmd('highlight clear')
if vim.fn.exists('syntax_on') == 1 then
    vim.cmd('syntax reset')
end

vim.g.colors_name = 'spaceway'
vim.opt.termguicolors = true

local g = vim.g
local highlight = vim.api.nvim_set_hl

local none = "NONE"

-- Palette from Emacs spaceway
local fg = "#CDCDCD"
local bg = "#000000"
local blue0 = "#7D8FA3"
local region = "#8fafd7"
local green0 = "#a1bf78"
local green1 = "#a1bf78"
local green2 = "#85A7A5"
local green3 = "#75b5aa"
local grey0 = "#888888"
local grey1 = "#7f7f7f"
local grey2 = "#151515"
local grey3 = "#1c1c1c"
local grey4 = "#363636"
local grey5 = "#444444"
local orange0 = "#E5C078"
local orange1 = "#f4bf75"
local purple0 = "#8197bf"
local purple1 = "#474e90"
local purple2 = "#9F7AA5"
local purple3 = "#aa759f"
local red0 = "#CC6666"
local red1 = "#af5f5f"
local red2 = "#d75f5f"
local red3 = "#ff73fd"
local yellow0 = "#E5C078"
local yellow1 = "#FFAF00"
local hl_line = "#14171B"

-- Terminal colors
vim.g.terminal_color_0  = bg
vim.g.terminal_color_1  = red1
vim.g.terminal_color_2  = green2
vim.g.terminal_color_3  = orange0
vim.g.terminal_color_4  = blue0
vim.g.terminal_color_5  = purple0
vim.g.terminal_color_6  = purple2
vim.g.terminal_color_7  = fg
vim.g.terminal_color_8  = grey3
vim.g.terminal_color_9  = red2
vim.g.terminal_color_10 = green3
vim.g.terminal_color_11 = yellow1
vim.g.terminal_color_12 = region
vim.g.terminal_color_13 = purple3
vim.g.terminal_color_14 = blue0
vim.g.terminal_color_15 = fg

-- Core highlight groups (expand as needed)
highlight(0, "Normal", { fg = fg, bg = bg })
highlight(0, "Comment", { fg = grey0, italic = true })
highlight(0, "Visual", { bg = region })
highlight(0, "CursorLine", { bg = hl_line })
highlight(0, "CursorLineNr", { fg = blue0, bg = hl_line })
highlight(0, "LineNr", { fg = grey1, bg = bg })
highlight(0, "StatusLine", { fg = grey1, bg = grey3 })
highlight(0, "StatusLineNC", { fg = grey1, bg = grey2 })
highlight(0, "VertSplit", { fg = grey4, bg = bg })
highlight(0, "Pmenu", { fg = fg, bg = grey3 })
highlight(0, "PmenuSel", { fg = bg, bg = blue0 })
highlight(0, "PmenuSbar", { bg = grey4 })
highlight(0, "PmenuThumb", { bg = grey5 })
highlight(0, "Search", { fg = bg, bg = orange0 })
highlight(0, "IncSearch", { fg = bg, bg = orange1 })
highlight(0, "String", { fg = green1 })
highlight(0, "Function", { fg = blue0 })
highlight(0, "Keyword", { fg = purple0, bold = true })
highlight(0, "Identifier", { fg = fg })
highlight(0, "Type", { fg = orange0 })
highlight(0, "Constant", { fg = orange1 })
highlight(0, "Error", { fg = red0, bg = bg, bold = true })
highlight(0, "WarningMsg", { fg = orange0, bold = true })
highlight(0, "DiffAdd", { fg = green0, bg = bg })
highlight(0, "DiffChange", { fg = yellow0, bg = bg })
highlight(0, "DiffDelete", { fg = red0, bg = bg })
highlight(0, "DiffText", { fg = fg, bg = hl_line })
highlight(0, "Directory", { fg = blue0, bg = bg })
highlight(0, "Special", { fg = purple2 })
highlight(0, "Todo", { fg = orange0, bold = true })
highlight(0, "Underlined", { fg = blue0, underline = true })
-- Add more highlight groups as needed, inspired by the Emacs theme
