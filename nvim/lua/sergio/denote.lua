-- Personal denote-style notes: thin wrapper around ~/notes/utils/denote.
-- No external plugin -- front matter is the source of truth, filenames are
-- always derived from it by the script, never typed by hand.

local NOTES_DIR = vim.fn.expand("~/notes")
local SLIPBOX_DIR = NOTES_DIR .. "/slipbox"
local DENOTE_BIN = NOTES_DIR .. "/utils/denote"

local function run(cmd)
  local out = vim.fn.system(cmd)
  return vim.trim(out), vim.v.shell_error
end

local function edit(path)
  vim.cmd.edit(vim.fn.fnameescape(path))
end

-- Parse `denote list-notes` output (path\ttitle\ttags per line) into a list
-- of { path, title, tags } tables. `tags` stays a "a, b" display string.
local function get_notes(dir)
  local cmd = { DENOTE_BIN, "list-notes" }
  if dir then
    table.insert(cmd, "--dir")
    table.insert(cmd, dir)
  end
  local out, code = run(cmd)
  if code ~= 0 then
    vim.notify("denote: " .. out, vim.log.levels.ERROR)
    return nil
  end
  local notes = {}
  for _, line in ipairs(vim.split(out, "\n", { trimempty = true })) do
    local path, title, tags = line:match("^([^\t]+)\t([^\t]*)\t(.*)$")
    if path then
      table.insert(notes, { path = path, title = title, tags = tags })
    end
  end
  return notes
end

local function note_display(note)
  if note.tags ~= "" then
    return note.title .. "  [" .. note.tags .. "]"
  end
  return note.title
end

local function note_entry_maker(note)
  return { value = note.path, display = note_display(note), ordinal = note.title .. " " .. note.tags }
end

local function new_note()
  vim.ui.input({ prompt = "Title: " }, function(title)
    if not title or title == "" then
      return
    end
    vim.ui.input({ prompt = "Tags (comma-separated): " }, function(tags)
      local cmd = { DENOTE_BIN, "new", title }
      if tags and tags ~= "" then
        table.insert(cmd, "-t")
        table.insert(cmd, tags)
      end
      local out, code = run(cmd)
      if code ~= 0 then
        vim.notify("denote: " .. out, vim.log.levels.ERROR)
        return
      end
      edit(out)
    end)
  end)
end

local function rename_current()
  vim.cmd("update")
  local current = vim.fn.expand("%:p")
  local out, code = run({ DENOTE_BIN, "rename", current })
  if code ~= 0 then
    vim.notify("denote: " .. out, vim.log.levels.ERROR)
    return
  end
  if out ~= current then
    edit(out)
  end
end

local function insert_link()
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")

  local current = vim.fn.expand("%:p")
  local notes = get_notes(nil) -- whole vault: link to anything, not just slipbox
  if not notes then
    return
  end

  pickers
    .new({}, {
      prompt_title = "Link to note",
      finder = finders.new_table({ results = notes, entry_maker = note_entry_maker }),
      sorter = conf.generic_sorter({}),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)
          if not selection then
            return
          end
          local target = NOTES_DIR .. "/" .. selection.value
          local link, lcode = run({ DENOTE_BIN, "link-to", current, target })
          if lcode ~= 0 then
            vim.notify("denote: " .. link, vim.log.levels.ERROR)
            return
          end
          vim.api.nvim_put({ link }, "c", true, true)
        end)
        return true
      end,
    })
    :find()
end

local function backlinks()
  local current = vim.fn.expand("%:p")
  local out, code = run({ DENOTE_BIN, "backlinks", current })
  if code ~= 0 then
    vim.notify("denote: " .. out, vim.log.levels.INFO)
    return
  end
  local qf = {}
  for _, line in ipairs(vim.split(out, "\n", { trimempty = true })) do
    local file, lnum, text = line:match("^([^:]+):(%d+): (.*)$")
    if file then
      table.insert(qf, { filename = NOTES_DIR .. "/" .. file, lnum = tonumber(lnum), text = text })
    end
  end
  if #qf == 0 then
    vim.notify("denote: no backlinks", vim.log.levels.INFO)
    return
  end
  vim.fn.setqflist(qf)
  vim.cmd("copen")
end

-- Fuzzy-find a slipbox note by title/tags (not raw filename) and open it.
local function find_notes()
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")

  local notes = get_notes(SLIPBOX_DIR)
  if not notes then
    return
  end

  pickers
    .new({}, {
      prompt_title = "Notes",
      finder = finders.new_table({ results = notes, entry_maker = note_entry_maker }),
      sorter = conf.generic_sorter({}),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)
          if selection then
            edit(SLIPBOX_DIR .. "/" .. selection.value)
          end
        end)
        return true
      end,
    })
    :find()
end

-- Pick one or more tags (<Tab> to multi-select), push every matching note
-- into the quickfix list.
local function find_by_tags()
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")

  local notes = get_notes(SLIPBOX_DIR)
  if not notes then
    return
  end

  local tag_set = {}
  for _, note in ipairs(notes) do
    for tag in note.tags:gmatch("[^,]+") do
      tag = vim.trim(tag)
      if tag ~= "" then
        tag_set[tag] = true
      end
    end
  end
  local tags = vim.tbl_keys(tag_set)
  table.sort(tags)
  if #tags == 0 then
    vim.notify("denote: no tags found", vim.log.levels.INFO)
    return
  end

  pickers
    .new({}, {
      prompt_title = "Tags (<Tab> to select multiple)",
      finder = finders.new_table({ results = tags }),
      sorter = conf.generic_sorter({}),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          local picker = action_state.get_current_picker(prompt_bufnr)
          local multi = picker:get_multi_selection()
          local selected = {}
          if #multi > 0 then
            for _, e in ipairs(multi) do
              selected[e.value] = true
            end
          else
            local e = action_state.get_selected_entry()
            if e then
              selected[e.value] = true
            end
          end
          actions.close(prompt_bufnr)
          if not next(selected) then
            return
          end

          local qf = {}
          for _, note in ipairs(notes) do
            for tag in note.tags:gmatch("[^,]+") do
              if selected[vim.trim(tag)] then
                table.insert(qf, {
                  filename = SLIPBOX_DIR .. "/" .. note.path,
                  lnum = 1,
                  text = note_display(note),
                })
                break
              end
            end
          end
          vim.fn.setqflist(qf)
          vim.cmd("copen")
        end)
        return true
      end,
    })
    :find()
end

vim.keymap.set("n", "<leader>nn", new_note, { desc = "denote: new note" })
vim.keymap.set("n", "<leader>nr", rename_current, { desc = "denote: rename to match front matter" })
vim.keymap.set("n", "<leader>nl", insert_link, { desc = "denote: insert link" })
vim.keymap.set("n", "<leader>nb", backlinks, { desc = "denote: backlinks (whole vault)" })
vim.keymap.set("n", "<leader>ns", find_notes, { desc = "denote: find note (title/tags)" })
vim.keymap.set("n", "<leader>nt", find_by_tags, { desc = "denote: find by tags -> quickfix" })
