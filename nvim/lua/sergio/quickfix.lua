-- :Cfilter[!] {pat} / :Lfilter[!] {pat} -- keep (or with !, drop) entries whose
-- file name or text matches {pat}. Ships with Vim; just needs loading.
-- Replaces the hand-rolled :QFilter / :QFilterFile commands.
vim.cmd.packadd("cfilter")

-- Nvim 0.11+ already maps ]q/[q (cnext/cprev), ]Q/[Q (clast/cfirst) and the
-- ]l/[l loclist equivalents. keybinds.lua adds <C-n>/<C-p> with centring.

local function toggle_quickfix()
  for _, win in pairs(vim.fn.getwininfo()) do
    if win.quickfix == 1 then
      vim.cmd.cclose()
      return
    end
  end
  vim.cmd.copen()
end

vim.keymap.set("n", "<leader>q", toggle_quickfix, { desc = "toggle quickfix" })
