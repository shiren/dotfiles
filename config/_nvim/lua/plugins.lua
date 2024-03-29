-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd([[packadd packer.nvim]])

return require("packer").startup(function(use)
	-- Packer can manage itself
	use("wbthomason/packer.nvim")

	use({
		"nvim-telescope/telescope.nvim",
		tag = "0.1.4",
		-- or                            , branch = '0.1.x',
		requires = {
			{ "nvim-lua/plenary.nvim" },
			{ "kdheepak/lazygit.nvim" },
			-- { "nvim-telescope/telescope-project.nvim" },
			{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
			{ "kyazdani42/nvim-web-devicons" },
		},
	})

	use({
		"rose-pine/neovim",
		as = "rose-pine",
		config = function()
			vim.cmd("colorscheme rose-pine")
		end,
	})

	use("nvim-treesitter/nvim-treesitter", { run = ":TSUpdate" })

	use("mbbill/undotree")

	use({
		"VonHeikemen/lsp-zero.nvim",
		requires = {
			-- LSP Support
			{ "neovim/nvim-lspconfig" },
			{ "williamboman/mason.nvim" },
			{ "williamboman/mason-lspconfig.nvim" },

			-- Autocompletion
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "hrsh7th/cmp-nvim-lua" },

			-- Snippets
			{ "L3MON4D3/LuaSnip" },

			-- Snippet Collection (Optional)
			-- {'rafamadriz/friendly-snippets'},
		},
	})

	use("jose-elias-alvarez/null-ls.nvim", {
		requires = { { "nvim-lua/plenary.nvim" } },
	})

	use("lewis6991/gitsigns.nvim", {
		requires = { { "nvim-lua/plenary.nvim" } },
	})

	use("kyazdani42/nvim-web-devicons")

	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
	})

	use({ "junegunn/fzf", run = "./install --bin" })

	use({
		"junegunn/fzf.vim",
	})

	use({
		"ahmedkhalf/project.nvim",
	})

	use({ "alexghergh/nvim-tmux-navigation" })

	use({
		"nvimdev/lspsaga.nvim",
		after = "nvim-lspconfig",
		requires = { { "nvim-tree/nvim-web-devicons" } },
		config = function()
			require("lspsaga").setup({})
		end,
	})

	-- use({
	-- 	"github/copilot.vim",
	-- })

	use({ "zbirenbaum/copilot.lua" })

	use({
		"zbirenbaum/copilot-cmp",
		require = { "copilot.lua" },
	})

	use({
		"onsails/lspkind.nvim",
		requires = { "nvim-cmp" },
	})

	-- Rust
	use("simrat39/rust-tools.nvim")
end)
