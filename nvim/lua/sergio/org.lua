-- nvim-orgmode: Emacs-style Org files in Neovim.
vim.pack.add({
  { src = "https://github.com/nvim-orgmode/orgmode" },
})

-- orgmode ships and manages its own `org` treesitter parser (it is not part of
-- the nvim-treesitter registry), installing it on first setup.
require("orgmode").setup({
  org_agenda_files = { "~/notes/org/**/*", "~/notes/gtd.org" },
  org_default_notes_file = "~/notes/gtd.org",
  -- default mappings: <leader>oa = agenda, <leader>oc = capture
  hyperlinks = {
    sources = {
      require("sergio.orgmode_denote_link"):new(),
    },
  },
})
