-- My own settings, in one place: knobs that are not Vim options, or that need
-- more than one value (colorcolumn: on/off at startup + where). Modules read
-- them with require("sergio.config"). Plain Vim options stay in settings.lua.
--
-- Some are changed at runtime for the session only (:Theme, <leader>tt);
-- edit this file to make a change permanent.
return {
  theme = {
    dark = "gruber-darker",  -- colorscheme for background=dark
    light = "vscode",        -- colorscheme for background=light
    transparent = true,      -- no background color (<leader>tt toggles it)
  },

  -- "native" (completions.lua + vim.lsp.completion) or "cmp" (cmp.lua)
  completion = "native",

  -- file extensions formatted with the LSP on save, e.g. { "lua", "go" }
  format_on_save = {},

  -- vertical guide line; <leader>tcc toggles it at `column`
  colorcolumn = {
    enabled = false,  -- shown at startup
    column = 100,
  },
}
