local M = {}

local context_file = ".chat_files"

local function append_to_file(path, line)
    local file = io.open(path, "a")
    if file then
        file:write(line .. "\n")
        file:close()
    else
        vim.notify("Error opening " .. path, vim.log.levels.ERROR)
    end
end

function M.add_current()
    local filepath = vim.api.nvim_buf_get_name(0)
    local rel_path = vim.fn.fnamemodify(filepath, ":.") 
    
    if rel_path ~= "" then
        append_to_file(context_file, rel_path)
        vim.notify("Added: " .. rel_path)
    end
end

function M.add_quickfix()
    local qflist = vim.fn.getqflist()
    if #qflist == 0 then
        vim.notify("Quickfix is empty", vim.log.levels.WARN)
        return
    end

    local unique_files = {}
    local count = 0

    for _, item in ipairs(qflist) do
        if item.bufnr and item.bufnr ~= 0 then
            local filepath = vim.api.nvim_buf_get_name(item.bufnr)
            local rel_path = vim.fn.fnamemodify(filepath, ":.")
            
            if rel_path ~= "" and not unique_files[rel_path] then
                unique_files[rel_path] = true
                append_to_file(context_file, rel_path)
                count = count + 1
            end
        end
    end
    vim.notify("Added " .. count .. " files from quickfix.")
end

function M.clear_context()
    os.remove(context_file)
    vim.notify("Context cleaned (" .. context_file .. ")")
end

-- local chat_ctx = require('sergio.context')
local chat_ctx = M

vim.keymap.set('n', '<leader>ca', chat_ctx.add_current, { desc = "Add current file to AI context" })
vim.keymap.set('n', '<leader>cq', chat_ctx.add_quickfix, { desc = "Add Quickfix to AI context" })
vim.keymap.set('n', '<leader>cl', chat_ctx.clear_context, { desc = "Clear AI context file" })
vim.keymap.set('n', '<leader>ce', ':vsplit .chat_files | edit!<CR>', { desc = "Edit context file" })

vim.keymap.set('n', '<leader>cx', ':!ctx_list<CR>', { desc = "Copy AI context to clipboard" })

return M

