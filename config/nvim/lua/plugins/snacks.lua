return {
  {
    "folke/snacks.nvim",
    keys = {
      {
        "<leader>ji",
        function()
          require("snacks").picker.lines({
            layout = {
              preset = "ivy", -- 하단 바 레이아웃
            },
          })
        end,
        desc = "lines",
      },
    },
  },
}
