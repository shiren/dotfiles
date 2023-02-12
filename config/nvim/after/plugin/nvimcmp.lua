local cmp = require("cmp")

local function complete()
	return function()
		if cmp.visible() then
			cmp.mapping.confirm({ select = true })()
		else
			vim.api.nvim_feedkeys(vim.fn["copilot#Accept"](), "i", true)
		end
	end
end

cmp.setup({
	mapping = {
		["<Tab>"] = cmp.mapping(complete(), { "i", "c" }),
	},
})

vim.g.copilot_no_tab_map = true
vim.g.copilot_assume_mapped = true
