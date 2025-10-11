-- lua/utils/feature_toggler.lua
-- A central place to manage on/off states for various features.

local M = {}

-- The "source of truth" for our setting.
-- Set the default value here. Let's default to OFF since it was causing issues.
M.image_previews_enabled = false

--- Toggles the inline image previews from snacks.nvim
function M.toggle_image_previews()
  -- 1. Flip the boolean state
  M.image_previews_enabled = not M.image_previews_enabled

  -- 2. Re-configure snacks.nvim with the new state
  -- We use pcall to safely require it, just in case.
  local success, snacks = pcall(require, "snacks")
  if not success then
    vim.notify("Could not find snacks.nvim to re-configure.", vim.log.levels.WARN)
    return
  end

  snacks.setup({
    image = {
      enabled = M.image_previews_enabled,
    },
  })

  -- 3. Give the user clear feedback
  if M.image_previews_enabled then
    vim.notify("✅ Inline image previews ENABLED", vim.log.levels.INFO, { title = "Snacks.nvim" })
  else
    vim.notify("❌ Inline image previews DISABLED", vim.log.levels.INFO, { title = "Snacks.nvim" })
  end
end

return M
