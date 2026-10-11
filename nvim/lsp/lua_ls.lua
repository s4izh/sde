---@type vim.lsp.Config
return {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml", ".git" },
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
      completion = {
        -- confirming a function completion inserts the call with its arguments
        -- as snippet placeholders (<Tab> / vim.snippet jumps between them)
        callSnippet = "Replace",
      },
    },
  },
}
