-- Put this in a file like lua/custom/notes.lua or directly in your init.lua

local M = {}

---@param opts table|nil Options table:
---             opts.extension string | nil The file extension to use (default: ".md")
---             opts.open_command string | nil Command to open the file (default: "edit", can be "split", "vsplit", etc.)
---             opts.sanitize_fn function | nil Custom function to sanitize the filename base.
---                                          Takes selected text, returns sanitized text.
function M.create_file_from_selection_or_cword(opts)
  opts = opts or {}
  local extension = opts.extension or ".md"
  local open_command = opts.open_command or "edit"

  local selected_text
  local mode = vim.fn.mode(1) -- Get current mode (e.g., "n", "v", "V")

  if mode:find("[vV]") or mode == "␖" then -- Visual, Visual Line, or Visual Block
    -- For visual mode, we can yank the selection to a register and get it.
    -- This is a common way, but it will clobber the unnamed register and exit visual mode.
    local original_visual_marks_valid = vim.fn.visualmode() ~= "" -- Check if visual marks are actually set

    if not original_visual_marks_valid then
         -- If visual mode was just entered but nothing selected, e.g. `v<esc>` then `callfunc`
         -- In this case, try cword as a fallback.
         selected_text = vim.fn.expand("<cword>")
         if not selected_text or selected_text == "" then
            vim.notify("No visual selection and no word under cursor.", vim.log.levels.WARN)
            return
         end
    else
        -- Save current content of unnamed register
        local old_reg_content = vim.fn.getreg('"')
        local old_reg_type = vim.fn.getregtype('"')

        vim.cmd('noautocmd normal! gv"vy') -- gv reselects, "vy yanks to unnamed register
        selected_text = vim.fn.getreg('"')

        -- Restore unnamed register
        vim.fn.setreg('"', old_reg_content, old_reg_type)
        vim.api.nvim_input("<Esc>") -- Exit visual mode
    end
  else -- Normal mode or other modes
    selected_text = vim.fn.expand("<cword>")
  end

  if not selected_text or selected_text == "" then
    vim.notify("No text selected or word under cursor.", vim.log.levels.WARN)
    return
  end

  selected_text = vim.trim(selected_text) -- Remove leading/trailing whitespace

  local filename_base
  if opts.sanitize_fn then
    filename_base = opts.sanitize_fn(selected_text)
  else
    -- Default sanitization:
    filename_base = selected_text:lower()
    filename_base = filename_base:gsub("%s+", "-")       -- Replace spaces (one or more) with a single hyphen
    filename_base = filename_base:gsub("[^%w%-%._]", "") -- Remove characters not alphanumeric, hyphen, underscore, or dot
    filename_base = filename_base:gsub("^%.+", "")       -- Remove leading dots (to prevent hidden files unintentionally)
    filename_base = filename_base:gsub("%.+$", "")       -- Remove trailing dots
    filename_base = filename_base:gsub("^-+", "")        -- Remove leading hyphens
    filename_base = filename_base:gsub("-+$", "")        -- Remove trailing hyphens
  end

  if filename_base == "" then
    vim.notify("Selected text resulted in an empty filename after sanitization.", vim.log.levels.WARN)
    return
  end

  local current_buf_path = vim.api.nvim_buf_get_name(0)
  local parent_dir

  if current_buf_path and current_buf_path ~= "" then
    parent_dir = vim.fn.fnamemodify(current_buf_path, ":h")
    if parent_dir == "." or parent_dir == "" then -- If file is in current dir or :h returns empty
        parent_dir = vim.fn.getcwd()
    end
  else
    parent_dir = vim.fn.getcwd()
    vim.notify("Current buffer is unnamed. Using Neovim's CWD: " .. parent_dir, vim.log.levels.INFO)
  end

  if not vim.fn.isdirectory(parent_dir) then
    -- This case is rare (e.g., if cwd was deleted after nvim started)
    -- Attempt to create it.
    vim.fn.mkdir(parent_dir, "p")
    if not vim.fn.isdirectory(parent_dir) then
        vim.notify("Parent directory does not exist and could not be created: " .. parent_dir, vim.log.levels.ERROR)
        return
    end
  end

  local new_file_path = parent_dir .. "/" .. filename_base .. extension

  -- Check if file already exists and ask if desired (optional)
  -- if vim.fn.filereadable(new_file_path) then
  --   local choice = vim.fn.input("File already exists. Overwrite? (y/N): ")
  --   if choice:lower() ~= "y" then
  --     vim.notify("File creation cancelled.", vim.log.levels.INFO)
  --     return
  --   end
  -- end

  vim.cmd(open_command .. " " .. vim.fn.fnameescape(new_file_path))
  vim.notify("Opened/Created: " .. new_file_path, vim.log.levels.INFO)
end

-- Example Usage (you can call this from a mapping or command)

-- 1. As a Lua function call (e.g., for a mapping)
-- require('custom.notes').create_file_from_selection_or_cword()

-- 2. Create a user command
vim.api.nvim_create_user_command(
  "CreateNote",
  function(args)
    local opts = {}
    if args.args and args.args ~= "" then
      opts.extension = args.args -- e.g., :CreateNote .txt
    end
    M.create_file_from_selection_or_cword(opts)
  end,
  {
    nargs = "?", -- Optional argument for extension
    desc = "Create file from selection/cword (default .md, pass extension like .txt)",
    -- Basic completion for extension argument
    complete = function(arg_lead, cmd_line, cursor_pos)
      if arg_lead:sub(1,1) == "." then
        return {".md", ".txt", ".rst", ".org", ".wiki"}
      end
      return {}
    end
  }
)

-- 3. Create keymaps
-- For visual mode selection
vim.keymap.set("v", "<leader>cn", function()
  M.create_file_from_selection_or_cword({ extension = ".md" })
end, { desc = "Create Note from Visual Selection (.md)" })

-- For word under cursor in normal mode
vim.keymap.set("n", "<leader>cn", function()
  M.create_file_from_selection_or_cword({ extension = ".md" })
end, { desc = "Create Note from cword (.md)" })

-- Example to create a .txt file instead:
vim.keymap.set("v", "<leader>ct", function()
  M.create_file_from_selection_or_cword({ extension = ".txt" })
end, { desc = "Create Txt from Visual Selection (.txt)" })

vim.keymap.set("n", "<leader>ct", function()
  M.create_file_from_selection_or_cword({ extension = ".txt" })
end, { desc = "Create Txt from cword (.txt)" })


return M
