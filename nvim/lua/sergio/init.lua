-- keymap registry first: it wraps vim.keymap.set for everything below
require("sergio.keymaps")

-- plugins first: vim.pack.add is synchronous, so everything below can require them
require("sergio.plugins")

-- core (no plugins)
require("sergio.settings")
require("sergio.autocommands")
require("sergio.completions")
require("sergio.keybinds")
require("sergio.grep")
-- completion engine: config.completion ("native" or "cmp")
if require("sergio.config").completion == "cmp" then
  require("sergio.cmp")
end
require("sergio.clipboard")
require("sergio.sudo")
require("sergio.whitespace")
require("sergio.quickfix")

-- plugin config
require("sergio.themes")
require("sergio.treesitter")
require("sergio.lsp")
require("sergio.telescope")
require("sergio.harpoon")
require("sergio.oil")
require("sergio.fugitive")
require("sergio.dispatch")
-- require("sergio.statusline")
require("sergio.denote")
require("sergio.refs")
require("sergio.org")
require("sergio.context")
require("sergio.clue")
