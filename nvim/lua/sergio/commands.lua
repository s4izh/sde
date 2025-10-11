vim.api.nvim_create_user_command('Scratch', function()
  vim.cmd 'below 10new'
  local buf = vim.api.nvim_get_current_buf()
  for name, value in pairs {
    filetype = 'scratch',
    buftype = 'nofile',
    bufhidden = 'hide',
    swapfile = false,
    modifiable = true,
  } do
    vim.api.nvim_set_option_value(name, value, { buf = buf })
  end
end, { desc = 'Open a scratch buffer', nargs = 0 })

local screenshot_manager = require('sergio.plugins.img-clip')

vim.api.nvim_create_user_command(
  'SetScreenshotDir',
  function(opts)
    screenshot_manager.set_session_path(opts.args)
  end,
  {
    nargs = 1,
    complete = 'dir',
    desc = 'Set the img-clip screenshot directory for the current session',
  }
)

-- Command to CLEAR the session directory
vim.api.nvim_create_user_command(
  'ClearScreenshotDir',
  screenshot_manager.clear_session_path,
  { desc = 'Clear the session screenshot directory and revert to default' }
)

-- Command to SHOW the currently active directory
vim.api.nvim_create_user_command(
  'ShowScreenshotDir',
  screenshot_manager.show_current_path,
  { desc = 'Show the active screenshot directory for img-clip' }
)

