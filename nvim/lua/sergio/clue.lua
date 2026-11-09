-- mini.clue: after a trigger key (<leader>, g, z, [, ], <C-w>, marks,
-- registers) waits a moment and shows the possible next keys with their desc.
-- Group names come from keymaps.group() calls in each module; the rest from
-- each mapping's desc (see :Keymaps). Loaded last so every group is declared.
local miniclue = require("mini.clue")
local keymaps = require("sergio.keymaps")

miniclue.setup({
  triggers = {
    { mode = "n", keys = "<Leader>" },
    { mode = "x", keys = "<Leader>" },
    { mode = "n", keys = "g" },
    { mode = "x", keys = "g" },
    { mode = "n", keys = "z" },
    { mode = "x", keys = "z" },
    { mode = "n", keys = "[" },
    { mode = "n", keys = "]" },
    { mode = "n", keys = "<C-w>" },
    { mode = "n", keys = "'" },
    { mode = "n", keys = "`" },
    { mode = "n", keys = '"' },
    { mode = "x", keys = '"' },
    { mode = "i", keys = "<C-r>" },
    { mode = "c", keys = "<C-r>" },
  },

  clues = {
    keymaps.clues(),

    miniclue.gen_clues.builtin_completion(),
    miniclue.gen_clues.g(),
    miniclue.gen_clues.marks(),
    miniclue.gen_clues.registers(),
    miniclue.gen_clues.windows(),
    miniclue.gen_clues.z(),
  },

  window = {
    delay = 700,
    -- which-key style: a panel along the bottom, full width, with a line on
    -- top (holds the title with the keys typed so far). Computed on every
    -- show so it follows resizes.
    config = function()
      local bottom = vim.o.lines - vim.o.cmdheight - (vim.o.laststatus > 0 and 1 or 0)
      return {
        anchor = "SW",
        row = bottom,
        col = 0,
        width = vim.o.columns,
        border = { "", "─", "", "", "", "", "", "" },  -- top line only
        title_pos = "center",
      }
    end,
    -- keep the panel open while scrolling long lists (<C-d>/<C-u>)
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  },
})
