require("telescope").setup({
	extensions = {
		repo = {
			list = {
				search_dirs = {
					"~/ws_dev",
				},
			},
		},
	},
})

require("telescope").load_extension("repo")

vim.keymap.set("n", "<leader>pP", require("telescope").extensions.repo.list, { silent = true, noremap = true })
