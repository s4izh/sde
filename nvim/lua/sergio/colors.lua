vim.opt.termguicolors = true
vim.opt.background = "dark"

vim.g.gruvbox_contrast_dark = 'hard'

function ColorMyPencils(color)
    color = color or "rose-pine"
    vim.cmd.colorscheme(color)
    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "SignColumn", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.api.nvim_set_hl(0, "CursorLineNr", { fg = "white", bg = "none" })
    vim.api.nvim_set_hl(0, "LineNr", { bg = "none", ctermbg = "none", fg = "grey" })

    vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
    -- vim.api.nvim_set_hl(0, "StatusLine", { fg = "white" })  -- Transparent status line
    vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none", fg = "grey" }) -- Inactive status line color

    -- Set the normal text color for inactive windows
    -- vim.api.nvim_set_hl(0, "NormalNC", { bg = "none", fg = "grey" })  -- Change 'grey' to your desired color
    vim.api.nvim_set_hl(0, "NormalNC", { bg = "none" })                 -- Change 'grey' to your desired color

    vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = "none", fg = "grey" }) -- Use 'grey' or your preferred color
end

local theme

if os.getenv("THEME_IS_LIGHT") then
    theme = "grey"
else
    theme = "gruber-darker"
end

ColorMyPencils(theme)

vim.api.nvim_create_user_command("Theme", function(opts)
    ColorMyPencils(opts.args)
end, {
    nargs = 1,
    complete = function(ArgLead, CmdLine, CursorPos)
        return vim.fn.getcompletion(ArgLead, 'color')
    end,
})
