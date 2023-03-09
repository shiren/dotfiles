local cmp = require("cmp")
local lspkind = require("lspkind")

-- local function complete(fallback)
-- 	return function()
-- 		local copilot_keys = vim.fn["copilot#Accept"]()
--
-- 		print(copilot_keys)
--
-- 		if cmp.visible() then
-- 			cmp.mapping.confirm({ select = true })()
-- 		elseif copilot_keys ~= "" and type(copilot_keys) == "string" then
-- 			-- vim.api.nvim_feedkeys(copilot_keys, "i", true)
-- 			print(copilot_keys)
-- 		else
-- 			fallback()
-- 		end
-- 	end
-- end
--
-- cmp.setup({
-- 	mapping = {
-- 		["<Tab>"] = cmp.mapping(complete(), { "i", "c" }),
-- 	},
-- })
-- vim.g.copilot_no_tab_map = true
-- vim.g.copilot_assume_mapped = true
--

local has_words_before = function()
	if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then
		return false
	end
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_text(0, line - 1, 0, line - 1, col, {})[1]:match("^%s*$") == nil
end
cmp.setup({
	mapping = {
		["<Tab>"] = vim.schedule_wrap(function(fallback)
			if cmp.visible() and has_words_before() then
				cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
			else
				fallback()
			end
		end),
	},
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol_text", -- show only symbol annotations
			maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
			ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
		}),
	},
})
