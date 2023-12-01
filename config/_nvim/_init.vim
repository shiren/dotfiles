set number
set relativenumber

syntax on
syntax sync fromstart

set t_Co=256
set lazyredraw

"C style indent
set cindent
set autoindent
set smartindent
set copyindent
"shift를 4칸으로 ( >, >>, <, << 등의 명령어)
set shiftwidth=2
"tab을 2칸으로
set tabstop=2
"tab 대신 띄어쓰기로
set expandtab

"검색시 대소문자 구별하지않음
"set ignorecase
"검색시 하이라이트(색상 강조)
set hlsearch

set nocompatible
"파일인코딩 형식 지정
set fileencodings=utf-8,euc-kr
set undolevels=1000
set tabpagemax=50
"제목을 표시
set title
"매칭되는 괄호를 보여줌
set showmatch
"자동 줄바꿈 하지 않음
set nowrap
set clipboard=unnamed
set foldmethod=indent
set nofoldenable
set showcmd

set mouse=a

set completeopt-=menuone,noselect

set cursorline

set notimeout

set hidden
set nobackup
set noswapfile

set autoread
set wildignore+=*/node_modules/**

"공백문자들
set list

"자연스러운 분할
"set splitbelow          " Horizontal split below current.
"set splitright          " Vertical split to right of current.

set ttyfast

" Make it obvious where 80 characters is
set textwidth=80
set colorcolumn=+1
set colorcolumn=80
" highlight ColorColumn guibg=#181818
" }}}
"


set langmenu=en_US.UTF-8
language messages en_US.UTF-8
set shell=/usr/local/bin/zsh

"빔 윈도우의 사이즈가 변경되었을때 = 자동 실행
autocmd VimResized * wincmd =

" open help vertically
command! -nargs=* -complete=help Help vertical belowright help <args>
autocmd FileType help wincmd L

set rtp+=/usr/local/opt/fzf

"=== Plugin ==="
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')
Plug 'tpope/vim-sensible'
" Plug 'kdheepak/lazygit.nvim'

" Search/Navigating
Plug 'pelodelfuego/vim-swoop'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Language Server Protocol
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/nvim-lsp-installer'
Plug 'glepnir/lspsaga.nvim'
Plug 'onsails/lspkind-nvim'

" File Management
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }

Plug 'ahmedkhalf/project.nvim'

Plug 'github/copilot.vim'
Plug 'folke/trouble.nvim'

Plug 'dracula/vim', { 'as': 'dracula' }

" Code/Edit
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'

Plug 'sbdchd/neoformat'
" Git
Plug 'lewis6991/gitsigns.nvim'

" UI
Plug 'kyazdani42/nvim-web-devicons'
Plug 'liuchengxu/vim-which-key'
call plug#end()

"=== KEYMAP ==="
"basic
let g:mapleader = "\<Space>"
imap jk <ESC>
imap ㅓㅏ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

"=== wich-key ==="
nnoremap <silent> <leader>? :silent WhichKey '<Space>'<CR>
nnoremap <silent> <leader> :silent WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :silent <c-u> :silent WhichKeyVisual '<Space>'<CR>


" Create map to add keys to
let g:which_key_map =  {}
" Define a separator
let g:which_key_sep = '→'
set timeoutlen=100

" Change the colors if you want
highlight default link WhichKey          Operator
highlight default link WhichKeySeperator DiffAdded
highlight default link WhichKeyGroup     Identifier
highlight default link WhichKeyDesc      Function

" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Single mappings
let g:which_key_map[' '] = [ ':Telescope find_files hidden=true' , 'find_files' ]
"let g:which_key_map['w'] = [ ':Files'                     , 'search files' ]

let g:which_key_map['f'] = {
    \ 'name' : '+file' ,
    \ 'r': [ ':Telescope oldfiles' , 'recent files' ],
    \ 'g': [ ':Telescope live_grep' , 'grep files' ],
    \ }

let g:which_key_map['p'] = {
    \ 'name' : '+projects',
    \ 'p': [ ':Telescope projects' , 'projects' ],
    \ }

let g:which_key_map['h'] = {
    \ 'name' : '+help',
    \ 'h': [ ':Telescope help_tags' , 'help tags' ],
    \ 'c': [ ':Telescope commands' , 'commands' ],
    \ }

let g:which_key_map.b = {
      \ 'name' : '+buffer' ,
      \ '1' : ['b1'        , 'buffer 1']        ,
      \ '2' : ['b2'        , 'buffer 2']        ,
      \ 'd' : ['bd'        , 'delete-buffer']   ,
      \ 'f' : ['bfirst'    , 'first-buffer']    ,
      \ 'h' : ['Startify'  , 'home-buffer']     ,
      \ 'l' : ['blast'     , 'last-buffer']     ,
      \ 'n' : ['bnext'     , 'next-buffer']     ,
      \ 'p' : ['bprevious' , 'previous-buffer'] ,
      \ 'b' : ['Buffers'   , 'fzf-buffer']      ,
      \ }

let g:which_key_map['c'] = {
    \ 'name' : '+code',
    \ 'e': [ ':lua vim.diagnostic.open_float()<cr>' , 'diagnostic open' ],
    \ 'q': [ ':lua vim.diagnostic.setloclist()' , 'setloclist' ],
    \ 'd': [ ':lua vim.lsp.buf.definition()' , 'definition' ],
    \ 'a': [ ':lua vim.lsp.buf.code_action()' , 'code action' ],
    \ 'f': [ ':lua vim.lsp.buf.format({async=true})' , 'formating' ],
    \ }

let g:which_key_map['j'] = {
    \ 'name' : '+navi',
    \ 'i': [ ':call Swoop()' , 'Swoop' ],
    \ 'k': [ ':call SwoopSelection()' , 'Swoop Selection' ],
    \ 'I': [ ':call SwoopMulti()' , 'Swoop Multi' ],
    \ 'K': [ ':call SwoopMultiSelection()' , 'Swoop Multi Selection' ],
    \ }

call which_key#register('<Space>', "g:which_key_map")

"=== cmp(autocomplete) ==="
lua <<EOF
  local cmp = require'cmp'
  local lspkind = require'lspkind'

  cmp.setup({
    snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body)
      end,
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-p>'] = cmp.mapping.scroll_docs(-4),
      ['<C-n>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.close(),
      ['<CR>'] = cmp.mapping.confirm({
        behavior = cmp.ConfirmBehavior.Replace,
        select = true
      }),
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
    }, {
      { name = 'buffer' },
    }),
    formatting = {
      format = lspkind.cmp_format({with_text = false, maxwidth = 50})
    }
  })

  vim.cmd [[highlight! default link CmpItemKind CmpItemMenuDefault]]
EOF

"LSP Setup"
"npm install -g typescript-language-server
lua << EOF

local opts = { noremap=true, silent=true }
--vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
--vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
--  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

 if client.name == 'tsserver' then
   client.server_capabilities.document_formatting = false
   client.server_capabilities.document_range_formatting = false
 end

 if client.name == 'diagnosticls' then
--   vim.cmd("nnoremap <silent><buffer> <Leader>cf :lua vim.lsp.buf.formatting()<CR>")
   vim.cmd("autocmd BufWritePost <buffer> lua vim.lsp.buf.format({async=true})")
 end

 if client.server_capabilities.document_range_formatting then
--   vim.cmd("xnoremap <silent><buffer> <Leader>cf :lua vim.lsp.buf.range_formatting({})<CR>")
 end

end

local capabilities = require('cmp_nvim_lsp').default_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)

local diagnosticls_config = {
  on_attach = on_attach,
  filetypes = { 'javascript', 'javascriptreact', 'json', 'typescript', 'typescriptreact', "typescript.tsx", 'css', 'less', 'scss', 'pandoc' },
  init_options = {
    linters = {
      eslint = {
        command = 'eslint_d',
        rootPatterns = { '.git' },
        debounce = 100,
        args = { '--stdin', '--stdin-filename', '%filepath', '--format', 'json' },
        sourceName = 'eslint_d',
        parseJson = {
          errorsRoot = '[0].messages',
          line = 'line',
          column = 'column',
          endLine = 'endLine',
          endColumn = 'endColumn',
          message = '[eslint] ${message} [${ruleId}]',
          security = 'severity'
        },
        securities = {
          [2] = 'error',
          [1] = 'warning'
        }
      },
    },
    filetypes = {
      javascript = 'eslint',
      javascriptreact = 'eslint',
      typescript = 'eslint',
      typescriptreact = 'eslint',
    },
    formatters = {
      eslint_d = {
        command = 'eslint_d',
        rootPatterns = { '.git' },
        args = { '--stdin', '--stdin-filename', '%filename', '--fix-to-stdout' },
        rootPatterns = { '.git' },
      },
      prettier = {
        command = 'prettier',
        rootPatterns = { '.git' },
        -- requiredFiles: { 'prettier.config.js' },
        args = { '--stdin', '--stdin-filepath', '%filename' }
      }
    },
    formatFiletypes = {
      css = 'prettier',
      scss = 'prettier',
      less = 'prettier',
      javascript = 'prettier',
      javascriptreact = 'prettier',
      typescript = 'prettier',
      typescriptreact = 'prettier',
      json = 'prettier',
    }
  }
}

local lsp_installer = pcall(require, "nvim-lsp-installer")
require("nvim-lsp-installer").on_server_ready(function(server)
  local opts = {
    capabilities = capabilities,
    on_attach = on_attach,
  }

  if server.name == 'tsserver' then
    opts.filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "typescript.tsx" }
  end

  if server.name == 'diagnosticls' then
    opts = diagnosticls_config
  end

  server:setup(opts)
end)

require("lspsaga").init_lsp_saga()
EOF

nnoremap <silent> <C-j> <Cmd>Lspsaga diagnostic_jump_next<CR>
nnoremap <silent> K <Cmd>Lspsaga hover_doc<CR>
inoremap <silent> <C-k> <Cmd>Lspsaga signature_help<CR>
nnoremap <silent> gh <Cmd>Lspsaga lsp_finder<CR>

"=== Plugin Setup ==="
lua <<EOF
--gitsign
require('gitsigns').setup()

--treesitter
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = false,
    disable = {},
  },
  ensure_installed = {
    "tsx",
    "json",
    "swift",
    "lua",
    "html",
    "css",
    "javascript",
    "typescript",
  }
}
EOF

"Swoop
let g:swoopIgnoreCase = 1
let g:swoopAutoInsertMode = 1
let g:swoopUseDefaultMappings = 0
"nmap <Leader>ji :call Swoop()<CR>
"vmap <Leader>ji :call SwoopSelection()<CR>
"nmap <Leader>jI :call SwoopMulti()<CR>
"vmap <Leader>jI :call SwoopMultiSelection()<CR>

"Telescope
lua << EOF
local telescope = require('telescope');
telescope.setup{ defaults = { file_ignore_patterns = {"node_modules", ".git/"} } }
telescope.load_extension('projects')
telescope.load_extension('fzf');
EOF


"nnoremap <leader><leader> <cmd>Telescope find_files<cr>
"nnoremap <leader>fg <cmd>Telescope live_grep<cr>
"nnoremap <leader>fb <cmd>Telescope buffers<cr>
"nnoremap <leader>fh <cmd>Telescope help_tags<cr>
"nnoremap <leader>pp <cmd>Telescope projects<cr>
"nnoremap <leader>fr <cmd>Telescope oldfiles<cr>
"nnoremap <leader>hc <cmd>Telescope commands<cr>

lua << EOF
  require("project_nvim").setup {
  -- Manual mode doesn't automatically change your root directory, so you have
  -- the option to manually do so using `:ProjectRoot` command.
  manual_mode = false,

  -- Methods of detecting the root directory. **"lsp"** uses the native neovim
  -- lsp, while **"pattern"** uses vim-rooter like glob pattern matching. Here
  -- order matters: if one is not detected, the other is used as fallback. You
  -- can also delete or rearangne the detection methods.
  detection_methods = { "lsp", "pattern" },

  -- All the patterns used to detect root dir, when **"pattern"** is in
  -- detection_methods
  patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },

  -- Table of lsp clients to ignore by name
  -- eg: { "efm", ... }
  ignore_lsp = {},

  -- Don't calculate root dir on specific directories
  -- Ex: { "~/.cargo/*", ... }
  exclude_dirs = {},

  -- Show hidden files in telescope
  show_hidden = false,

  -- When set to false, you will get a message when project.nvim changes your
  -- directory.
  silent_chdir = true,

  -- Path where project.nvim will store the project history for use in

  datapath = vim.fn.stdpath("data"),  }
EOF

"neoformat
"let g:neoformat_try_node_exe = 1
"
"lua << EOF
"vim.api.nvim_create_autocmd("BufWritePre", {
"  pattern = { "*.js", "*.jsx", "*.ts", "*.tsx" },
"  command = "Neoformat prettier",
"})
"EOF

"========================= ETC ==================================
"저장시 필요없는 스페이스 지우기
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

autocmd FileType * autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileType * autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FileType * autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd FileType * autocmd BufWritePre     * :call TrimWhiteSpace()

"Change cursor at each mode (command, insert)
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
if exists('$ITERM_PROFILE')
  if exists('$TMUX')
    let &t_SI="\<Esc>[3 q"
    let &t_EI="\<Esc>[0 q"
  else
    let &t_SI="\<Esc>]50;CursorShape=1\x7"
    let &t_EI="\<Esc>]50;CursorShape=0\x7"
  endif
end

" Colors {{{
"if (has("termguicolors"))
"  set termguicolors " enable true colors support
"endif
colorscheme dracula
set background=dark " light or dark
" colorscheme onebuddy

highlight Cursor guifg=#f00 guibg=#657b83
" highlight Comment cterm=italic gui=italic

"Color override"
"Avoid hidden Errors by cursorLine"
hi Error cterm=reverse ctermbg=white ctermfg=red
