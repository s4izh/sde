-- Keymap registry: records every mapping made from this config, warns about
-- conflicts and lists them all with :Keymaps. Modules keep calling
-- vim.keymap.set as usual; this wraps it, so it must be required first.
--
--   :Keymaps   all config mappings grouped by prefix, with desc and file:line
--              (gF jumps to the definition); conflicts and missing desc on top
--
-- Prefix groups are named next to their mappings, in the module that owns them:
--
--   local keymaps = require("sergio.keymaps")
--   keymaps.group("<leader>g", "+git")
--
-- The names show in :Keymaps and in mini.clue (clue.lua reads M.clues()).
--
-- A conflict is the same mode + lhs mapped from two different places in the
-- config, in the same scope (global, or the same buffer). The later one wins
-- silently in Neovim, so they are reported at startup.

local M = {}

local config_dir = vim.fn.resolve(vim.fn.stdpath("config"))

-- entries[key] = { mode, lhs, desc, buffer, defs = { "file:line", ... } }
M.entries = {}

-- groups = { { modes = { "n" }, lhs = "<leader>g", desc = "+git" }, ... }
M.groups = {}

function M.group(lhs, desc, modes)
  if type(modes) == "string" then modes = { modes } end
  table.insert(M.groups, { lhs = lhs, desc = desc, modes = modes or { "n", "x" } })
end

-- groups in mini.clue's format
function M.clues()
  local out = {}
  for _, g in ipairs(M.groups) do
    for _, mode in ipairs(g.modes) do
      table.insert(out, { mode = mode, keys = g.lhs, desc = g.desc })
    end
  end
  return out
end


local function normalize(lhs)
  -- <Leader>/<leader>, <C-A>/<c-a> are the same key
  return (lhs:gsub("<[^>]+>", string.lower))
end

-- file:line (relative to the config dir) of whoever called vim.keymap.set /
-- nvim_set_keymap, or nil when that is a plugin or the runtime. Only the direct
-- caller counts: maps a plugin creates inside a setup() called from the config
-- (mini.clue triggers, ...) belong to the plugin.
local function config_caller()
  local info = debug.getinfo(3, "Sl")
  if not info or info.source:sub(1, 1) ~= "@" then return nil end
  local path = vim.fn.resolve(info.source:sub(2))
  if path:sub(1, #config_dir + 1) ~= config_dir .. "/" then return nil end
  return path:sub(#config_dir + 2) .. ":" .. info.currentline
end

local function record(modes, lhs, opts, def)
  opts = opts or {}
  if type(modes) == "string" then modes = { modes } end
  local buffer = opts.buffer
  if buffer == true or buffer == 0 then buffer = vim.api.nvim_get_current_buf() end
  local scope = buffer and ("buf" .. buffer) or "global"
  for _, mode in ipairs(modes) do
    local key = table.concat({ scope, mode == "" and " " or mode, normalize(lhs) }, "\0")
    local e = M.entries[key]
    if not e then
      e = { mode = mode, lhs = lhs, buffer = buffer ~= nil, defs = {} }
      M.entries[key] = e
    end
    e.desc = opts.desc or e.desc
    if not vim.tbl_contains(e.defs, def) then
      table.insert(e.defs, def)
    end
  end
end

local keymap_set = vim.keymap.set
vim.keymap.set = function(modes, lhs, rhs, opts)
  local def = config_caller()
  if def then record(modes, lhs, opts, def) end
  return keymap_set(modes, lhs, rhs, opts)
end

-- also catch the raw API (after/plugin/tmux-navigator.lua uses it); calls made
-- by vim.keymap.set itself come from the runtime and are skipped
local api_set = vim.api.nvim_set_keymap
vim.api.nvim_set_keymap = function(mode, lhs, rhs, opts)
  local def = config_caller()
  if def then record(mode, lhs, opts, def) end
  return api_set(mode, lhs, rhs, opts)
end

function M.conflicts()
  local out = {}
  for _, e in pairs(M.entries) do
    if #e.defs > 1 then table.insert(out, e) end
  end
  table.sort(out, function(a, b) return a.lhs < b.lhs end)
  return out
end

-- group: "<leader>t" for "<leader>tW", "g" for "gd" (first key of a sequence),
-- and one shared group for single keys (<C-d>, -, ...)
local SINGLE = "~ teclas sueltas"  -- "~" sorts it after the rest
local function group_of(lhs)
  local n = normalize(lhs)
  local first, rest = n:match("^(<[^>]+>)(.*)$")
  if not first then first, rest = n:sub(1, 1), n:sub(2) end
  if first == "<leader>" or first == "<localleader>" then
    local second = rest:match("^(<[^>]+>)") or rest:sub(1, 1)
    -- <leader>x alone: shared group; <leader>x... (a prefix): its own group
    if rest == second then return first .. " (sueltas)" end
    return first .. second
  end
  if rest == "" then return SINGLE end
  return first
end

function M.lines()
  local lines = {}
  local function add(s) table.insert(lines, s) end
  local function row(e)
    local scope = e.buffer and " (buffer)" or ""
    return string.format("  %-3s %-18s %-45s %s", e.mode, e.lhs .. scope,
      e.desc or "-- sin desc --", table.concat(e.defs, ", "))
  end

  local all = vim.tbl_values(M.entries)
  -- buffer-local maps repeat per buffer: show each definition once
  local seen, uniq = {}, {}
  for _, e in ipairs(all) do
    local k = table.concat({ e.mode, normalize(e.lhs), tostring(e.buffer), e.defs[1] }, "\0")
    if not seen[k] then seen[k] = true; table.insert(uniq, e) end
  end

  local conflicts = M.conflicts()
  if #conflicts > 0 then
    add("# Conflictos (la última definición gana)")
    for _, e in ipairs(conflicts) do add(row(e)) end
    add("")
  end

  local missing = vim.tbl_filter(function(e) return not e.desc end, uniq)
  add(string.format("# %d atajos, %d sin desc", #uniq, #missing))
  add("")

  local groups = {}
  for _, e in ipairs(uniq) do
    local g = group_of(e.lhs)
    groups[g] = groups[g] or {}
    table.insert(groups[g], e)
  end
  local names = vim.tbl_keys(groups)
  table.sort(names)
  local group_names = {}
  for _, grp in ipairs(M.groups) do group_names[normalize(grp.lhs)] = grp.desc end

  for _, g in ipairs(names) do
    local list = groups[g]
    table.sort(list, function(a, b)
      if a.lhs == b.lhs then return a.mode < b.mode end
      return a.lhs < b.lhs
    end)
    add("## " .. g .. (group_names[g] and ("  " .. group_names[g]) or ""))
    for _, e in ipairs(list) do add(row(e)) end
    add("")
  end
  return lines
end

vim.api.nvim_create_user_command("Keymaps", function()
  vim.cmd("botright new")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, M.lines())
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].modifiable = false
  vim.bo[buf].filetype = "markdown"
  vim.opt_local.path = config_dir .. "/**"  -- so gF on "lua/x.lua:12" works
  vim.opt_local.wrap = false
  vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf, desc = "Close :Keymaps" })
end, { desc = "List config keymaps with conflicts and sources" })

-- report conflicts once everything (including after/plugin) is loaded
vim.api.nvim_create_autocmd("VimEnter", {
  once = true,
  callback = function()
    local c = M.conflicts()
    if #c == 0 then return end
    local msg = { "Atajos definidos dos veces (ver :Keymaps):" }
    for _, e in ipairs(c) do
      table.insert(msg, string.format("  %s %s: %s", e.mode, e.lhs, table.concat(e.defs, ", ")))
    end
    vim.notify(table.concat(msg, "\n"), vim.log.levels.WARN)
  end,
})

return M
