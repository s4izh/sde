return {
  "David-Kunz/gen.nvim",
  config = function()
    local gen = require("gen")
    gen.model = 'codellama'
    vim.keymap.set({ 'n', 'v' }, '<leader>G', ':Gen<CR>')
    gen.prompts = {
      document = {
        prompt =
        "Document the code in doxygen format, just write the comments nothing else: ```$text```",
        replace = true,
        extract = "```$filetype\n(.-)```"
      },
      -- document = {
      --   prompt =
      --   "Document the code in doxygen format ```$filetype\n...\n```:\n```$filetype\n$text\n```",
      --   replace = true,
      --   extract = "```$filetype\n(.-)```"
      -- },
    }
  end
}
