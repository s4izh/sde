-- vim-dispatch: run builds/tests asynchronously and collect the results in the
-- quickfix list (see quickfix.lua for navigating it).
--
--   :Make [args]        run 'makeprg' in the background
--   :Dispatch {cmd}     run any command, quickfix-aware
--   :Dispatch           re-run the last one
--   :Start {cmd}        run without capturing output
--   :Copen              open the dispatch output
vim.pack.add({
  { src = "https://github.com/tpope/vim-dispatch" },
})

-- open the command-line window filtered to previous :Dispatch invocations, so
-- an earlier build can be picked and re-run
vim.keymap.set("n", "<Leader>cr", "q:?Dispatch<cr><cr>", { desc = "rerun a previous :Dispatch" })
vim.keymap.set("n", "<Leader>cm", "<cmd>Make<cr>", { desc = ":Make (async)" })
