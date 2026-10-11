-- help i_CTRL_X, ins-completion
-- menuone: show menu even for one match
-- noselect: don't pre-highlight the first item
-- noinsert: don't insert a candidate's text while navigating until confirmed
-- fuzzy:    fuzzy candidate matching (native completion, Nvim 0.11+)
-- (When nvim-cmp is active it overrides completeopt with its own value.)
vim.opt.completeopt = "menu,menuone,noinsert,noselect,fuzzy"
