return {
  'nvim-telescope/telescope.nvim',
  enabled = false,
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function ()
    require('sergio.plugins.telescope')
  end
}
