-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()

require('orgmode').setup({
  org_agenda_files = {'~/docs/org/*', '~/docs/my-orgs/**/*'},
  org_default_notes_file = '~/docs/org/refile.org',
  org_hide_leading_stars = true,
  org_hide_emphasis_markers = true,
})

-- headlines

-- require("headlines").setup {
--     org = {
--         fat_headlines = false,
--     },
-- }
