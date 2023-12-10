local rust_tools = {
  'simrat39/rust-tools.nvim',
  config = function()
    local rt = require("rust-tools")
    rt.setup({
      server = {
        on_attach = function(_, bufnr)
          -- Hover actions
          vim.keymap.set("n", ",h", rt.hover_actions.hover_actions, { buffer = bufnr })
          -- Code action groups
          vim.keymap.set("n", ",a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
      },
    })
  end
}

local crates = {
  'saecki/crates.nvim',
  tag = 'stable',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    -- remove icons https://github.com/Saecki/crates.nvim/wiki/Stable-documentation
    local crates = require('crates')
    crates.setup({
      popup = {
        border = 'rounded',
      },
    })
    local opts = { silent = true }
    vim.api.nvim_create_autocmd("BufRead", {
      group = vim.api.nvim_create_augroup("CmpSourceCargo", { clear = true }),
      pattern = "Cargo.toml",
      callback = function()
        vim.keymap.set('n', ',cv', crates.show_versions_popup, opts)
        vim.keymap.set('n', ',cf', crates.show_features_popup, opts)
        vim.keymap.set('n', ',cd', crates.show_dependencies_popup, opts)
        vim.keymap.set('n', ',cu', crates.update_crate, opts)
        vim.keymap.set('v', ',cu', crates.update_crates, opts)
        vim.keymap.set('n', ',ca', crates.update_all_crates, opts)
        vim.keymap.set('n', ',cU', crates.upgrade_crate, opts)
        vim.keymap.set('v', ',cU', crates.upgrade_crates, opts)
        vim.keymap.set('n', ',cA', crates.upgrade_all_crates, opts)

        vim.keymap.set('n', ',ce', crates.expand_plain_crate_to_inline_table, opts)
        vim.keymap.set('n', ',cE', crates.extract_crate_into_table, opts)

        vim.keymap.set('n', ',cH', crates.open_homepage, opts)
        vim.keymap.set('n', ',cR', crates.open_repository, opts)
        vim.keymap.set('n', ',cD', crates.open_documentation, opts)
        vim.keymap.set('n', ',cC', crates.open_crates_io, opts)
      end,
    })
  end,
}

return {
  rust_tools,
  crates,
}
