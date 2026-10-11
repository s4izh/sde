---@type vim.lsp.Config
return {
  cmd = { "gopls" },
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_markers = { "go.work", "go.mod", ".git" },
  settings = {
    gopls = {
      -- confirming a function completion fills in its arguments as snippet
      -- placeholders instead of just the name
      usePlaceholders = true,
    },
  },
}
