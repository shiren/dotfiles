local cmp = require("cmp")
local lspkind = require("lspkind")
local cmp_action = require("lsp-zero").cmp_action()

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
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	mapping = {
		["<Tab>"] = vim.schedule_wrap(function(fallback)
			if cmp.visible() and has_words_before() then
				cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
			else
				fallback()
			end
		end),
		["<CR>"] = cmp.mapping.confirm({ select = true, behavior = cmp.ConfirmBehavior.Replace }),
	},
	formatting = {
		format = lspkind.cmp_format({
			mode = "symbol_text", -- show only symbol annotations
			maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
			ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
		}),
	},
	sources = {
		-- { name = "copilot",                group_index = 3,   keyword_length = 4 },
		{ name = "copilot", keyword_length = 3 },
		{ name = "path" }, -- file paths
		{ name = "nvim_lsp", keyword_length = 1 }, -- from language server
		{ name = "nvim_lsp_signature_help" }, -- display function signatures with current parameter emphasized
		{ name = "nvim_lua", keyword_length = 2 }, -- complete neovim's Lua runtime API such vim.lsp.*
		{ name = "buffer", keyword_length = 2 }, -- source current buffer
		{ name = "calc" },
	},
})
