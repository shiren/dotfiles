require("remap")
require("plugins")

local set = vim.opt -- set options
set.tabstop = 2
set.softtabstop = 2
set.shiftwidth = 2
set.expandtab = true

set.number = true
set.relativenumber = true

set.smartindent = true

set.wrap = false

set.swapfile = false
set.backup = false

set.termguicolors = true

set.scrolloff = 8
set.updatetime = 50

set.colorcolumn = "90"

set.clipboard = "unnamedplus"

-- lsp code action 때문에 거터 안움직이게
vim.opt.signcolumn = "yes"

vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
vim.opt.shortmess = vim.opt.shortmess + { c = true }
vim.api.nvim_set_option("updatetime", 300)

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])
