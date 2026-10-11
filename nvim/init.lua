vim.g.mapleader   = " "
vim.opt.guicursor = ""

-- Escape hatch for machines where the plugins come from somewhere else (Nix,
-- distro packages...): with NVIM_NO_PACK=1 nothing is downloaded or written to
-- ~/.local/share/nvim.
--
--   NVIM_NO_PACK=1 nvim          one-off
--   environment.variables        to make it permanent on NixOS
--
-- The packadd is what keeps this usable with the current layout: vim.pack
-- installs into pack/core/opt, and it is vim.pack.add() that puts those on the
-- runtimepath, so dropping it entirely would stop every already-installed
-- plugin from loading. Plugins provided by Nix as `start` packages are on the
-- runtimepath already and the packadd is simply a no-op for them.
if vim.env.NVIM_NO_PACK == "1" then
  vim.pack.add = function(specs)
    for _, spec in ipairs(specs or {}) do
      local src = type(spec) == "table" and spec.src or spec
      local name = type(spec) == "table" and spec.name
        or src:gsub("%.git$", ""):match("([^/]+)$")
      pcall(vim.cmd, "packadd " .. name)
    end
  end
end

require("sergio")
