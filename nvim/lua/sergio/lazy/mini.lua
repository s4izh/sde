return {
  {
    'nvim-mini/mini.files',
    version = '*',
    config = function ()
      require('mini.files').setup()
    end
  },
  {
    'nvim-mini/mini.pick',
    version = '*',
    config = function ()
      require('mini.pick').setup()
    end
  },
}
