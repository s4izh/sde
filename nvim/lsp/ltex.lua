---@type vim.lsp.Config
return {
  cmd = { "ltex-ls" },
  filetypes = { "tex", "bib" },
  root_markers = { ".git" },
  settings = {
    ltex = {
      language = "en",
    },
  },
}
