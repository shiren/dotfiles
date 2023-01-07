require('gitsigns').setup {
  signcolumn = true,
  numhl = true,
  on_attach = function(bufnr)
    -- Text object
    vim.keymap.set('o', 'ih', ':<C-U>Gitsigns select_hunk<CR>')
    vim.keymap.set('x', 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  end,
}
