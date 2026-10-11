-- Native LSP (no nvim-lspconfig). Per-server configs live in the top-level
-- `lsp/<name>.lua` files, which Neovim loads from the runtimepath. This module
-- enables the servers whose executables are installed and wires up the shared
-- attach behaviour (keymaps, completion, inlay hints, diagnostics).

-- server config name (matches lsp/<name>.lua) -> executable that must exist
local servers = {
  rust_analyzer = "rust-analyzer",
  gopls         = "gopls",
  bashls        = "bash-language-server",
  zls           = "zls",
  clangd        = "clangd",
  lua_ls        = "lua-language-server",
  ltex          = "ltex-ls",
  texlab        = "texlab",
  nixd          = "nixd",
  verible       = "verible-verilog-ls",
  marksman      = "marksman",
  vhdl_ls       = "vhdl_ls",
}

for name, exe in pairs(servers) do
  if vim.fn.executable(exe) == 1 then
    vim.lsp.enable(name)
  end
end

-- python: prefer pyright, fall back to pylsp
if vim.fn.executable("pyright-langserver") == 1 then
  vim.lsp.enable("pyright")
elseif vim.fn.executable("pylsp") == 1 then
  vim.lsp.enable("pylsp")
end

-- Buffer-local maps, installed on LspAttach and removed again on LspDetach so
-- that stopping a server restores the built-in meaning of gd/K/gi/... instead
-- of erroring with "method ... is not supported by any server".
local signature_help = function()
  vim.lsp.buf.signature_help({ border = "rounded" })
end

-- Nvim 0.11+ maps these itself whenever a server attaches -- do not redefine:
--
--   grn        rename                    gra   code action (also visual)
--   grr        references                gri   implementation
--   grt        type definition           grx   run codelens
--   gO         document symbol           K     hover
--   <C-S>      signature help (insert and select mode)
--
-- and these commands:
--
--   :lsp enable|disable|restart|stop [name]     :checkhealth vim.lsp
--
-- and it points these buffer options at the LSP:
--
--   tagfunc     -> <C-]> jumps to definition, <C-t> comes back
--   formatexpr  -> gq formats through the server
--   omnifunc    -> <C-x><C-o> completes
--
-- Snippets (0.11+ defaults, insert and select mode):
--
--   <Tab> / <S-Tab>   jump between placeholders while a snippet is active
--
-- Only what has no built-in equivalent is mapped below.
local lsp_keymaps = {
  { "n", "<Leader>ls", "<cmd>lsp stop<cr>" },
  { "n", "<Leader>lrr", "<cmd>lsp restart<cr>" },
  -- <C-]> also goes to the definition via tagfunc; gd is kept as the familiar key
  { "n", "gd", vim.lsp.buf.definition },
  { "n", "gD", vim.lsp.buf.declaration },
  -- <C-S> covers insert and select mode, but not normal mode
  { "n", "gs", signature_help },
  { "n", "<space>lf", function() vim.lsp.buf.format({ async = true }) end },
  { "n", "<leader>lwa", vim.lsp.buf.add_workspace_folder },
  { "n", "<leader>lwr", vim.lsp.buf.remove_workspace_folder },
  { "n", "<leader>lws", function() vim.lsp.buf.workspace_symbol() end },
  { "n", "<leader>lwl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end },
}

-- Diagnostics are not tied to a running server (they can come from other
-- sources), so these stay global and are set once, not per attach.
vim.keymap.set("n", "[d", function() vim.diagnostic.jump({ count = -1, float = true }) end)
vim.keymap.set("n", "]d", function() vim.diagnostic.jump({ count = 1, float = true }) end)
vim.keymap.set("n", "<leader>lq", vim.diagnostic.setloclist)
vim.keymap.set("n", "<leader>le", vim.diagnostic.open_float)

-- file extensions to format on save (empty = disabled)
local auto_format_extensions = {}

-- Inlay hints for any server that supports them: on by default, hidden in
-- insert mode, toggled with <leader>lh.
local function setup_inlay_hints(bufnr)
  vim.b[bufnr].inlay_hints_enabled = true

  vim.defer_fn(function()
    local mode = vim.api.nvim_get_mode().mode
    vim.lsp.inlay_hint.enable(mode == "n" or mode == "v", { bufnr = bufnr })
  end, 500)

  vim.keymap.set("n", "<leader>lh", function()
    vim.b[bufnr].inlay_hints_enabled = not vim.b[bufnr].inlay_hints_enabled
    vim.lsp.inlay_hint.enable(vim.b[bufnr].inlay_hints_enabled, { bufnr = bufnr })
    print("Inlay hints: " .. (vim.b[bufnr].inlay_hints_enabled and "ON" or "OFF"))
  end, { buffer = bufnr, desc = "Toggle Inlay Hints" })

  local group = vim.api.nvim_create_augroup("sergio/toggle_inlay_hints", { clear = false })
  vim.api.nvim_create_autocmd("InsertEnter", {
    group = group,
    buffer = bufnr,
    callback = function()
      vim.lsp.inlay_hint.enable(false, { bufnr = bufnr })
    end,
  })
  vim.api.nvim_create_autocmd("InsertLeave", {
    group = group,
    buffer = bufnr,
    callback = function()
      if vim.b[bufnr].inlay_hints_enabled then
        vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
      end
    end,
  })
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)

    -- native LSP autocompletion (see completions.lua) + omnifunc fallback.
    -- Skipped when nvim-cmp is the active engine (see cmp.lua).
    vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
    if vim.g.completion_engine ~= "cmp"
      and client and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = false })
    end

    -- inlay hints only for servers that opt in via `inlay_hints = true` in
    -- their lsp/<name>.lua (avoids grey "ghost text" from e.g. clangd)
    if client and client.server_capabilities.inlayHintProvider
      and client.config.inlay_hints then
      setup_inlay_hints(ev.buf)
    end

    for _, m in ipairs(lsp_keymaps) do
      vim.keymap.set(m[1], m[2], m[3], { buffer = ev.buf })
    end

    vim.api.nvim_set_hl(0, "FloatBorder", { link = "Normal" })

    local ext = vim.fn.expand("%:e")
    if vim.tbl_contains(auto_format_extensions, ext) then
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = "*." .. ext,
        callback = function()
          vim.lsp.buf.format({ async = true })
        end,
      })
    end
  end,
})

-- Drop the buffer-local maps once the last server detaches, so gd/K/gi/... go
-- back to their built-in behaviour and the LSP-only maps stop existing.
vim.api.nvim_create_autocmd("LspDetach", {
  group = vim.api.nvim_create_augroup("UserLspDetach", {}),
  callback = function(ev)
    local others = vim.tbl_filter(function(c)
      return c.id ~= ev.data.client_id and not c:is_stopped()
    end, vim.lsp.get_clients({ bufnr = ev.buf }))

    if #others > 0 then
      return -- another server is still attached; keep the maps
    end

    for _, m in ipairs(lsp_keymaps) do
      pcall(vim.keymap.del, m[1], m[2], { buffer = ev.buf })
    end
  end,
})

vim.diagnostic.config({
  float = { border = "rounded" },
})
