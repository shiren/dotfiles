vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<leader>w", vim.cmd.w)
vim.keymap.set("i", "jk", "<Esc>", { noremap = true })
vim.keymap.set({ "n", "i", "v" }, "<C-g>", "<Esc>", { noremap = true })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

vim.api.nvim_create_user_command("W", ":write", {})
