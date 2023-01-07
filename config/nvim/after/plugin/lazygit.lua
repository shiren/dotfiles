require("telescope").load_extension("lazygit")

vim.api.nvim_create_autocmd("BufEnter", {
	callback = function()
		require("lazygit.utils").project_root_dir()
	end,
})

vim.keymap.set("n", "<leader>m", vim.cmd.LazyGit, {})
