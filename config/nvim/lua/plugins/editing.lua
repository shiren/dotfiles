return {
  {
    "mg979/vim-visual-multi",
    event = "BufRead",
    config = function()
      vim.g.VM_maps = {
        ["Move Cursor Down"] = "<M-Down>",
        ["Move Cursor Up"] = "<M-Up>",
        -- 필요한 경우 추가적인 키맵 변경
      }
    end,
  },
}
