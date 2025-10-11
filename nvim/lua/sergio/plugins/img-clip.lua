local M = {}

M.default_opts = {
  default = {
    path = "./assets/images",
    name = "%Y-%m-%d-%H-%M-%S",
  },
}

M.session_path = nil

function M.setup()
  local success, img_clip = pcall(require, "img-clip")
  if success then
    img_clip.setup(M.default_opts)
  end
end

--- @param path string The new directory path.
function M.set_session_path(path)
  local expanded_path = vim.fn.expand(path)
  vim.fn.mkdir(expanded_path, "p")

  if vim.fn.isdirectory(expanded_path) == 0 then
    vim.notify("Error: Could not create or find directory: " .. expanded_path, vim.log.levels.ERROR)
    return
  end

  M.session_path = expanded_path

  -- Create a new configuration table by overriding the default path
  local new_opts = vim.deepcopy(M.default_opts)
  new_opts.default.path = M.session_path

  -- Re-configure the plugin with the new path
  require("img-clip").setup(new_opts)
  vim.notify("Screenshot directory set to: " .. M.session_path, vim.log.levels.INFO)
end

--- Clears the session path and reverts to the default behavior.
function M.clear_session_path()
  M.session_path = nil
  -- Re-configure the plugin with the original default options
  require("img-clip").setup(M.default_opts)
  vim.notify("Screenshot directory cleared. Reverted to default.", vim.log.levels.INFO)
end

--- Shows the currently active screenshot directory.
function M.show_current_path()
  if M.session_path then
    vim.notify("Current screenshot directory: " .. M.session_path .. " (session override)")
  else
    vim.notify("Current screenshot directory: " .. M.default_opts.default.path .. " (default, relative to file)")
  end
end

return M
