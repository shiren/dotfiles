return {
  {
    "williamboman/mason.nvim",
    opts = {
      --- https://mason-registry.dev/registry/list
      ensure_installed = {
        "html-lsp",
        "json-lsp",
        "eslint-lsp",
        "tailwindcss-language-server",
        "lua-language-server",
        "vim-language-server",
        "yaml-language-server",
        "rust-analyzer",
        "typescript-language-server",
        "css-lsp",
        "stylua",
        "shellcheck",
      },
    },
  },
}
