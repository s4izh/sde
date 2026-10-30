local telescope = require("telescope")

-- standard telescope look (defaults) + fzf-native for a fast compiled sorter
telescope.setup({
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },
})

-- Load the fzf-native extension, building its C library first if it is missing
-- (safety net in case the PackChanged build hook did not run yet).
local function fzf_native_dir()
  -- info = false: only the path is needed, the default asks git for the
  -- revision of every plugin and costs ~300ms on each startup
  for _, p in ipairs(vim.pack.get(nil, { info = false })) do
    if p.spec.name == "telescope-fzf-native.nvim" then
      return p.path
    end
  end
end

local dir = fzf_native_dir()
if dir then
  if vim.fn.filereadable(dir .. "/build/libfzf.so") == 0 then
    vim.notify("Building telescope-fzf-native...", vim.log.levels.INFO)
    vim.fn.system({ "make", "-C", dir })
  end
  pcall(telescope.load_extension, "fzf")
end

local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, {})
vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})
vim.keymap.set("n", "<leader>fp", builtin.git_files, {})
vim.keymap.set("n", "<leader>fb", builtin.buffers, {})
vim.keymap.set("n", "<leader>fh", builtin.help_tags, {})
vim.keymap.set("n", "<leader>fs", builtin.grep_string, {})
vim.keymap.set("n", "<leader>fS", function()
  builtin.grep_string({ search = vim.fn.input("Grep > ") })
end)
vim.keymap.set("n", "<leader>fc", function()
  builtin.colorscheme({ enable_preview = true })
end, { desc = "pick colorscheme (live preview)" })
