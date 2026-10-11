-- Bib reference browser: fuzzy-find any bib entry across the vault (Zotero
-- exports, videos.bib, etc.) and jump to its denote note, creating one on the
-- spot (tagged "bib", front matter `reference: <citekey>`) if it doesn't
-- exist yet. denote.nvim has no notion of bib entries, so this is separate.

local function refs_bin()
  return vim.fn.expand("~/notes/utils/refs")
end

local function refs_open()
  local pickers = require("telescope.pickers")
  local finders = require("telescope.finders")
  local conf = require("telescope.config").values
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")

  local output = vim.fn.system({ refs_bin(), "list" })
  if vim.v.shell_error ~= 0 then
    vim.notify("refs: " .. output, vim.log.levels.ERROR)
    return
  end

  local entries = {}
  for _, line in ipairs(vim.split(output, "\n", { trimempty = true })) do
    local key, label = line:match("^([^\t]+)\t(.*)$")
    if key then
      table.insert(entries, { key = key, label = label })
    end
  end

  pickers
    .new({}, {
      prompt_title = "References",
      finder = finders.new_table({
        results = entries,
        entry_maker = function(entry)
          return { value = entry, display = entry.label, ordinal = entry.label }
        end,
      }),
      sorter = conf.generic_sorter({}),
      attach_mappings = function(prompt_bufnr, _)
        actions.select_default:replace(function()
          local selection = action_state.get_selected_entry()
          actions.close(prompt_bufnr)
          if not selection then
            return
          end
          local result = vim.fn.system({ refs_bin(), "create", selection.value.key })
          if vim.v.shell_error ~= 0 then
            vim.notify("refs: " .. result, vim.log.levels.ERROR)
            return
          end
          vim.cmd.edit(vim.fn.fnameescape(vim.trim(result)))
        end)
        return true
      end,
    })
    :find()
end

vim.keymap.set("n", "<leader>nf", refs_open, { desc = "refs: find/create note from bib entry" })
