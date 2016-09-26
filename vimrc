"required
filetype off

"set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
"Plugin
Plugin 'gmarik/Vundle.vim'

"File/Buffer
Plugin 'scrooloose/nerdtree'
    let NERDTreeQuitOnOpen=1
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'ctrlp.vim'
    let g:ctrlp_working_path_mode = 'ra'
    let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist\|lib\|report\|build'
    let g:ctrlp_mru_files = 1
    let g:ctrlp_dont_split = 'NERD_tree_2'
    let g:ctrlp_map = '<Leader>p'
    let g:ctrlp_prompt_mappings = {
        \ 'PrtSelectMove("j")':   ['<c-n>'],
        \ 'PrtSelectMove("k")':   ['<c-p>'],
        \ 'PrtHistory(-1)':       ['<up>'],
        \ 'PrtHistory(1)':        ['<down>'],
    \ }
Plugin 'rking/ag.vim'
Plugin 'jeetsukumaran/vim-buffergator'
    let g:buffergator_suppress_keymaps = 1

"Git
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'sjl/gundo.vim'

"Javascript
"Plugin 'node.js'
"Plugin 'pangloss/vim-javascript'
"Plugin 'Enhanced-Javascript-syntax'
Plugin 'othree/yajs.vim'
Plugin 'othree/es.next.syntax.vim'
Plugin 'gavocanov/vim-js-indent'
"Plugin 'everedifice/vim-js-syntax'
Plugin 'othree/jsdoc-syntax.vim'
Plugin 'javascript-libraries-syntax'
    let g:used_javascript_libs = 'jquery,underscore,jasmine,react'
Plugin '1995eaton/vim-better-javascript-completion'
Plugin 'othree/jspc.vim'
Plugin 'moll/vim-node'
Plugin 'heavenshell/vim-jsdoc'
    let g:jsdoc_default_mapping = 0
    let g:jsdoc_underscore_private = 1
Plugin 'marijnh/tern_for_vim'
    let tern_show_argument_hint='on_move'
    let tern_show_signature_in_pum=1
    let g:tern_map_keys=1
    let g:tern_map_prefix='<Leader>'

"C
Plugin 'c.vim'

"C#
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'tpope/vim-dispatch' "dependency for omnisharp-vim
    augroup omnisharp_commands
        autocmd!
        "Set autocomplete function to OmniSharp (if not using YouCompleteMe completion plugin)
        "autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

        " Synchronous build (blocks Vim)
        "autocmd FileType cs nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>

        " automatic syntax check on events (TextChanged requires Vim 7.4)
        autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

        " Automatically add new cs files to the nearest project on save
        autocmd BufWritePost *.cs call OmniSharp#AddToProject()

        "show type information automatically when the cursor stops moving
        autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()
    augroup END
    "Timeout in seconds to wait for a response from the server
    let g:OmniSharp_timeout = 1

"markdown"
Plugin 'plasticboy/vim-markdown'

"Lint/Autocomplete
if has('nvim')
Plugin 'benekastah/neomake'
    autocmd! BufWritePost * Neomake
    let g:neomake_javascript_enabled_makers = ['eslint']
else
"Plugin 'scrooloose/syntastic'
    "set statusline+=%#warningmsg#
    "set statusline+=%{SyntasticStatuslineFlag()}
    "set statusline+=%*
    "let g:syntastic_always_populate_loc_list = 1
    "let g:syntastic_auto_loc_list = 0
    "let g:syntastic_check_on_open = 1
    "let g:syntastic_check_on_wq = 0
    "let g:syntastic_javascript_checkers=["eslint"]
    "let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']

Plugin 'maralla/validator.vim'
    let g:validator_javascript_checkers=['eslint']
    let g:validator_filetype_map={'javascript.jsx': 'javascript'}
    let g:validator_warning_symbol='✓'
    let g:validator_error_symbol='✗'
    highlight ValidatorErrorSign ctermbg=18 ctermfg=09
    highlight ValidatorWarningSign ctermbg=18 ctermfg=12
Plugin 'Valloric/YouCompleteMe'
    let g:ycm_confirm_extra_conf=0
    let g:ycm_auto_trigger = 1
endif

"Colorthemes
Plugin 'chriskempson/base16-vim'
    let base16colorspace=256

"Editing
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdcommenter'

"Navigate
Plugin 'easymotion/vim-easymotion'
Plugin 'Shougo/unite-outline'

"ETC
Plugin 'SirVer/ultisnips'
    let g:UltiSnipsExpandTrigger="<c-e>"
    let g:UltiSnipsListSnippets="<c-l>"
    let g:UltiSnipsEditSplit="vertical"
Plugin 'tpope/vim-obsession'
Plugin 'bling/vim-airline'
    "show branch
    let g:airline#extensions#branch#enabled = 1
    "Enable the list of buffers
    let g:airline#extensions#tabline#enabled = 1
    "Show just the filename
    let g:airline#extensions#tabline#fnamemod = ':t'
    let g:airline#extensions#tabline#buffer_nr_show = 1
Plugin 'Shougo/unite.vim'
Plugin 'vim-xkbswitch'
    let g:XkbSwitchEnabled = 1
Plugin 'itchyny/vim-cursorword'
Plugin 'godlygeek/tabular'
Plugin 'Yggdroot/indentLine'
    let g:indentLine_color_term = 18
call vundle#end()            " required

"========================= Configuration ==================================
"required
filetype plugin indent on

"문법 하이라이트"
syntax on
syntax sync fromstart

set t_Co=256
set lazyredraw

"검정배경을 사용할 때, (이 색상에 맞춰 문법 하이라이트 색상이 달라집니다.)
set background=dark
colorscheme base16-tomorrow

"Show line number.
set number
set relativenumber

"C style indent
set cindent
set autoindent
set smartindent
set copyindent
"shift를 4칸으로 ( >, >>, <, << 등의 명령어)
set shiftwidth=4
"tab을 4칸으로
set tabstop=4
"tab 대신 띄어쓰기로
set expandtab

set laststatus=2
"검색시 대소문자 구별하지않음
"set ignorecase
"검색시 하이라이트(색상 강조)
set hlsearch

set incsearch

"방향키로 이동가능
set nocompatible
"파일인코딩 형식 지정
set fileencodings=utf-8,euc-kr
"backspace 키 사용 가능
set backspace=indent,eol,start
"명령어에 대한 히스토리를 1000개까지
set history=1000
set undolevels=1000
set tabpagemax=50
"상태표시줄에 커서의 위치 표시
set ruler
"제목을 표시
set title
"매칭되는 괄호를 보여줌
set showmatch
"자동 줄바꿈 하지 않음
set nowrap
"tab 자동완성시 가능한 목록을 보여줌
set wildmenu
set clipboard=unnamed
set foldmethod=indent
set nofoldenable
set showcmd
set smarttab

set mouse=a

set completeopt-=preview
set complete-=i

set ttimeout
set ttimeoutlen=100

set nrformats-=octal

set cursorline

set notimeout

set hidden
set nobackup
set noswapfile

set autoread

"공백문자들
set list
set listchars=tab:≈.,trail:·,extends:ø,nbsp:+

"자연스러운 분할
set splitbelow          " Horizontal split below current.
set splitright          " Vertical split to right of current.

set ttyfast

set langmenu=en_US.UTF-8
language messages en_US.UTF-8

set shell=/usr/local/bin/zsh

set sessionoptions-=options

"빔 윈도우의 사이즈가 변경되었을때 = 자동 실행
autocmd VimResized * wincmd =

" open help vertically
command! -nargs=* -complete=help Help vertical belowright help <args>
autocmd FileType help wincmd L

"======================= File Type settings ========================

au BufNewFile,BufRead *.vim setlocal noet ts=4 sw=4 sts=4
au BufNewFile,BufRead *.txt setlocal noet ts=4 sw=4
au BufNewFile,BufRead *.md setlocal noet ts=4 sw=4
au BufNewFile,BufRead *.yml,*.yaml setlocal expandtab ts=2 sw=2
au BufNewFile,BufRead *.cpp setlocal expandtab ts=2 sw=2
au BufNewFile,BufRead *.hpp setlocal expandtab ts=2 sw=2
au BufNewFile,BufRead *.json setlocal expandtab ts=2 sw=2

augroup filetypedetect
  au BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux
  au BufNewFile,BufRead .nginx.conf*,nginx.conf* setf nginx
augroup END

"========================= KEYMAP ==================================
"basic
let mapleader="\<Space>"
imap jk <ESC>
imap ㅓㅏ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

"Toggl Paste"
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

"=== Leader ====
nnoremap <Leader>s :w<CR>

nmap <silent> <Leader>/ :nohlsearch<CR>

nmap <silent> <leader>p :CtrlP<cr>
nmap <silent> <leader>e :CtrlPBuffer<cr>
nmap <silent> <leader>r :CtrlPMRUFiles<cr>

nmap <silent> <leader>n :NERDTreeToggle<CR>
nmap <silent> <leader>l :NERDTreeFind<CR>

nmap <leader>f <Plug>(easymotion-overwin-f)
nmap <leader>g <Plug>(easymotion-overwin-f2)
nmap <leader>l <Plug>(easymotion-overwin-line)
nmap <leader>w <Plug>(easymotion-overwin-w)

nmap <silent> <leader>u :Unite<CR>
nmap <silent> <leader>o :Unite outline<CR>

nmap <silent> <leader>gs :Gstatus<CR>
nmap <silent> <leader>gd :Gdiff<CR>
nmap <silent> <leader>gl :Glog -30<CR>

nmap <silent> <leader>bb :BuffergatorOpen<CR>

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

"Color override"
"Avoid hidden Errors by cursorLine"
hi Error cterm=reverse ctermbg=white ctermfg=red
