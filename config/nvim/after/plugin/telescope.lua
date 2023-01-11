local telescope = require("telescope")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")

vim.keymap.set("n", "<leader>pf", builtin.find_files, {})
vim.keymap.set("n", "<leader>fg", builtin.live_grep, {})

vim.keymap.set("n", "<C-p>", builtin.git_files, {})

vim.keymap.set("n", "<leader>bb", builtin.buffers, {})

vim.keymap.set("n", "<leader>ht", builtin.help_tags, {})

telescope.setup({
  defaults = {
    file_ignore_patterns = { "node_modules", ".git/" },
    mappings = {
      i = {
        ["<C-g>"] = actions.close,
      },
      n = {
        ["<C-g>"] = actions.close,
      },
    },
  },
  pickers = {
    find_files = {
      hidden = true,
    },
  },
})
