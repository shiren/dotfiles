local lsp = require("lsp-zero")

lsp.preset("recommended")

-- lsp.ensure_installed({
-- 	"tsserver",
-- 	"cssls",
-- 	"html",
-- 	"jsonls",
-- 	"eslint",
-- 	"tailwindcss",
-- 	"lua_ls",
-- 	"vimls",
-- 	"yamlls",
-- })

lsp.setup()

-- local cmp = require("cmp")
-- local cmp_action = require("lsp-zero").cmp_action()
--
-- cmp.setup({
-- 	window = {
-- 		completion = cmp.config.window.bordered(),
-- 		documentation = cmp.config.window.bordered(),
-- 	},
-- 	mapping = cmp.mapping.preset.insert({ -- 요거 lspsaga하고 충돌난다
-- 		["<C-Space>"] = cmp.mapping.complete(),
-- 		["<C-f>"] = cmp_action.luasnip_jump_forward(),
-- 		["<C-b>"] = cmp_action.luasnip_jump_backward(),
-- 		["<C-p>"] = cmp.mapping.scroll_docs(-4),
-- 		["<C-n>"] = cmp.mapping.scroll_docs(4),
-- 	}),
-- 	sources = {
-- 		{ name = "copilot", group_index = 2 },
-- 		{ name = "nvim_lsp", keyword_length = 2 },
-- 		{ name = "buffer", keyword_length = 2 },
-- 		{ name = "path", keyword_length = 2 },
-- 	},
-- })

-- lsp.setup_nvim_cmp({
-- 	sources = {
-- 		{ name = "copilot", group_index = 2 },
-- 		{ name = "nvim_lsp", keyword_length = 2 },
-- 		{ name = "buffer", keyword_length = 2 },
-- 		{ name = "path", keyword_length = 2 },
-- 	},
-- })

lsp.configure("lua_ls", {
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim", "require" },
			},
		},
	},
})

lsp.on_attach(function(client, bufnr)
	lsp.default_keymaps({ buffer = bufnr })
	-- -- Mappings.
	-- -- See `:help vim.lsp.*` for documentation on any of the below functions
	-- local bufopts = { noremap = true, silent = true, buffer = bufnr }

	-- --vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
	-- -- vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
	-- -- vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
	-- --vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
	-- vim.keymap.set("n", "<C-h>", vim.lsp.buf.signature_help, bufopts)
	-- vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
	-- vim.keymap.set("n", "<C-h>", vim.lsp.buf.signature_help, bufopts)
	-- vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, bufopts)
	-- vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, bufopts)
	-- vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
	-- vim.keymap.set("n", "<space>wl", function()
	-- 	print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
	-- end, bufopts)
	-- vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, bufopts)
	-- -- vim.keymap.set("n", "<space>gr", vim.lsp.buf.rename, bufopts)
	-- -- vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
	-- vim.keymap.set("n", "gf", vim.lsp.buf.references, bufopts)
	-- vim.keymap.set("n", "<space>f", function()
	-- 	vim.lsp.buf.format({ async = true })
	-- end, bufopts)

	-- -- vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, bufopts)
	-- -- vim.keymap.set("n", "]d", vim.diagnostic.goto_next, bufopts)
	-- vim.keymap.set("n", "gl", vim.diagnostic.open_float, bufopts)
end)

require("mason").setup({})
require("mason-lspconfig").setup({
	ensure_installed = {
		"tsserver",
		"cssls",
		"html",
		"jsonls",
		"eslint",
		"tailwindcss",
		"lua_ls",
		"vimls",
		"yamlls",
		"rust_analyzer",
	},
	handlers = {
		lsp.default_setup,
	},
})

-- lsp.nvim_workspace()
