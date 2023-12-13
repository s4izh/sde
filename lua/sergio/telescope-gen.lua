-- telescope-gen-nvim.lua

local M = {}

local actions = require('telescope.actions')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values

-- Define a list of available gen.nvim models
local models = {
    'model1',
    'model2',
    'model3',
    -- Add more models as needed
}

-- Function to set the selected gen.nvim model
local function set_gen_nvim_model(prompt_bufnr)
    local selection = actions.get_selected_entry(prompt_bufnr)
    if selection then
        require("gen").gen.model = selection.value
    end
end

-- Function to create a Telescope picker for gen.nvim models
function M.select_gen_nvim_model()
    pickers.new({}, {
        prompt_title = 'Select gen.nvim model',
        finder = finders.new_table({
            results = models,
        }),
        sorter = conf.generic_sorter({}),
        attach_mappings = function(_, map)
            map('i', '<CR>', set_gen_nvim_model)
            map('n', '<CR>', set_gen_nvim_model)
            return true
        end,
    }):find()
end

return M

