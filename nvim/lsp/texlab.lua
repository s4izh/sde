---@type vim.lsp.Config
return {
  cmd = { "texlab" },
  filetypes = { "tex", "bib", "plaintex" },
  root_markers = { ".latexmkrc", ".git" },
}
