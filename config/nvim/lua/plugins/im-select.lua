return {
  {
    "keaising/im-select.nvim",
    config = function()
      require("im_select").setup({
        default_command = "/opt/homebrew/bin/im-select",
        -- macOS에서 영문 키보드의 기본 ID
        default_im_select = "com.apple.keylayout.ABC",
        -- 인서트 모드로 돌아갈 때 이전 언어(한글)로 복구할지 여부
        set_previous_events = { "InsertEnter" },
      })
    end,
  },
}
