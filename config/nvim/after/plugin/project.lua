require("telescope").load_extension("project")

vim.keymap.set("n", "<leader>pp", require("telescope").extensions.project.project, {})
