local oil = require("oil")

-- Only overrides vs oil's defaults are set here; `columns` is toggled at runtime.
local config = {
  default_file_explorer = true,
  columns = {},
  view_options = {
    show_hidden = true,
  },
}

oil.setup(config)

vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- Toggle a detailed column view (icon/permissions/size/mtime) inside oil.
local detailed = false
local function toggle_oil_details()
  detailed = not detailed
  config.columns = detailed and { "icon", "permissions", "size", "mtime" } or {}
  oil.setup(config)
  require("oil.actions").refresh.callback()
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "oil",
  callback = function(args)
    vim.keymap.set("n", "gd", toggle_oil_details, { buffer = args.buf, desc = "Toggle Oil details" })
  end,
})
