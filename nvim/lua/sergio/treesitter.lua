-- nvim-treesitter `main` branch API (Nvim 0.11+).
-- The old `require("nvim-treesitter.configs").setup{}` framework is gone:
--   * parsers are installed via `.install()`
--   * highlighting is started per-buffer with `vim.treesitter.start()`
--   * indentation uses `.indentexpr()`
local ts = require("nvim-treesitter")

ts.setup()

-- Parsers to keep installed. markdown/markdown_inline are needed so LSP hover
-- floats render without crashing.
local ensure_installed = {
  "c", "cpp", "lua", "rust", "vimdoc",
  "markdown", "markdown_inline",
  "yaml", "toml", "bash", "json", "xml", "html", "python", "nix", "systemverilog",
}

-- ```sv fences are common in ~/notes but `sv` is not a known alias, so map it
-- onto the systemverilog parser.
vim.treesitter.language.register("systemverilog", "sv")

-- Where parsers come from depends on the machine:
--   * NixOS  -- declared in the system config, dropped on the runtimepath
--   * Ubuntu -- compiled on demand by nvim-treesitter into ~/.local/share/nvim
--
-- So "missing" means no parser/<lang>.so anywhere on the runtimepath, which
-- covers both sources. ts.get_installed() only knows about the second one and
-- would recompile grammars Nix already provides.
--
-- Set vim.g.ts_auto_install = false to never compile (e.g. a box without a C
-- compiler); parsers provided by the system are still used.
local function is_available(lang)
  return #vim.api.nvim_get_runtime_file("parser/" .. lang .. ".so", false) > 0
end

if vim.g.ts_auto_install ~= false then
  local missing = vim.tbl_filter(function(lang)
    return not is_available(lang)
  end, ensure_installed)

  if #missing > 0 then
    ts.install(missing)
  end
end

-- Start highlighting + indentation for buffers that have a parser available
-- (bundled with Nvim or installed above).
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("sergio/treesitter", { clear = true }),
  callback = function(args)
    local buf = args.buf

    if vim.bo[buf].filetype == "cmake" then
      return -- highlighting was disabled for cmake in the old config
    end

    if not pcall(vim.treesitter.start, buf) then
      return -- no parser for this filetype
    end

    vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
