
local action_state = require('telescope.actions.state') -- lua/Telescope/actions/state.lua
require('telescope').load_extension('fzf')

    -- Sample mapping in telescope
    -- luafile % to load the file
require('telescope').setup{
    defaults  = {
        mappings = {
            i = {
                ["<c-a>"] = function() print(vim.inspect(action_state.get_selected_entry())) end
            }
        }
    }
}

local mappings = {
}
--mappings.curr_buf = function()
    --local opt = require('telescope.themes').get_ivy()
    --require('telescope.builtin').current_buffer_fuzzy_find(opt)
--end
mappings.curr_buf = function()
    local opt = require('telescope.themes').get_dropdown({height=10, previewer=false, winblend=10})
    require('telescope.builtin').current_buffer_fuzzy_find(opt)
end
return mappings
