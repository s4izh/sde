return {
  "David-Kunz/gen.nvim",
  config = function()
    local gen = require("gen")
    gen.model = 'codellama'
    vim.keymap.set({ 'n', 'v' }, '<leader>G', ':Gen<CR>')
    gen.prompts['doxygen'] = {
        prompt = "Write the doxygen documentation for this code,\
        just write the doxygen documentation, don't output anything else,\
        I only want the comments that go into my file, don't output anything else, in format ```$filetype\n...\n```: \
        \n```$filetype\n$text\n```",
        replace = true,
        extract = "```$filetype\n(.-)```"
    }
    -- gen.prompts = {
    --   document = {
    --     prompt =
    --     "Document the code in doxygen format, just write the comments nothing else: ```$text```",
    --     replace = true,
    --     extract = "```$filetype\n(.-)```"
    --   },
      -- document = {
      --   prompt =
      --   "Document the code in doxygen format ```$filetype\n...\n```:\n```$filetype\n$text\n```",
      --   replace = true,
      --   extract = "```$filetype\n(.-)```"
      -- },
    -- }
  end
}
