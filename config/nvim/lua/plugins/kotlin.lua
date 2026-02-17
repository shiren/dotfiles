return {
  -- LSP(언어 서버) 설정 수정
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        kotlin_language_server = {
          settings = {
            kotlin = {
              compiler = { jvm = { target = "21" } }, -- 본인 자바 버전
            },
          },
        },
      },
    },
  },

  -- 포매터(ktlint) 강제 지정
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        kotlin = { "ktlint" },
      },
    },
  },

  -- 스프링 부트 실행용 디버거(DAP) 단축키/설정
  {
    "mfussenegger/nvim-dap",
    opts = function()
      local dap = require("dap")
      dap.configurations.kotlin = {
        {
          type = "kotlin",
          request = "launch",
          name = "Spring Boot Run",
          projectRoot = "${workspaceFolder}",
          mainClass = function()
            return vim.fn.input("Main Class: ", "com.example.DemoApplicationKt")
          end,
        },
      }
    end,
  },
}
