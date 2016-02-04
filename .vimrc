filetype off                  " required

" set the runtime path to include Vundle and initialize
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
Plugin 'rking/ag.vim'
Plugin 'jeetsukumaran/vim-buffergator'

"Git
Plugin 'tpope/vim-fugitive'
Plugin 'mhinz/vim-signify'
    "Plugin 'sjl/gundo.vim'

"Javascript
    "Plugin 'node.js'
Plugin 'pangloss/vim-javascript'
    "Plugin 'gavocanov/vim-js-indent'
    "Plugin 'Enhanced-Javascript-syntax'
Plugin 'javascript-libraries-syntax'
    let g:used_javascript_libs = 'jquery,underscore,jasmine,requirejs'
Plugin 'heavenshell/vim-jsdoc'
    let g:jsdoc_default_mapping = 0
    let g:jsdoc_underscore_private = 1
Plugin 'marijnh/tern_for_vim'
    let tern_show_argument_hint='on_move'
    let tern_show_signature_in_pum=1
    "let g:tern_map_keys=1

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
Plugin 'jtratner/vim-flavored-markdown'
    augroup markdown
        au!
        au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
    augroup END

"Lint/Autocomplete
Plugin 'benekastah/neomake'
if has('nvim')
    autocmd! BufWritePost * Neomake
    let g:neomake_javascript_enabled_makers = ['eslint']
endif
Plugin 'scrooloose/syntastic'
    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 0
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    let g:syntastic_javascript_checkers=["eslint"]
    let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']
if has('nvim')
    let g:syntastic_mode_map = { "mode": "passive"}
endif

Plugin 'Valloric/YouCompleteMe'
    let g:ycm_confirm_extra_conf=0
    set completeopt-=preview
    let g:ycm_auto_trigger = 1

"Colorthemes
Plugin 'cocopon/iceberg.vim'
Plugin 'nanotech/jellybeans.vim'
Plugin 'chriskempson/base16-vim'
    let base16colorspace=256

"Editing
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdcommenter'

"Navigate
Plugin 'EasyMotion'
Plugin 'Shougo/unite-outline'

"ETC
Plugin 'tpope/vim-obsession'
Plugin 'bling/vim-airline'
    set laststatus=2
    "show branch
    let g:airline#extensions#branch#enabled = 1
    "Enable the list of buffers
    let g:airline#extensions#tabline#enabled = 1
    "Show just the filename
    let g:airline#extensions#tabline#fnamemod = ':t'
    let g:airline#extensions#tabline#buffer_nr_show = 1
Plugin 'Shougo/unite.vim'
Plugin 'rizzatti/dash.vim'
Plugin 'vim-xkbswitch'
    let g:XkbSwitchLib = '/usr/local/lib/libxkbswitch.dylib'
    let g:XkbSwitchEnabled = 1
    let g:XkbSwitchNLayout = 'us'
call vundle#end()            " required

"========================= Configuration ==================================
filetype plugin indent on    " required

"문법 하이라이트"
syntax on
syntax sync fromstart

set t_Co=256
set lazyredraw


"검정배경을 사용할 때, (이 색상에 맞춰 문법 하이라이트 색상이 달라집니다.)
set background=dark
colorscheme base16-default

"Show line number.
set number

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
set bs=indent,eol,start
"명령어에 대한 히스토리를 1000개까지
set history=1000
set undolevels=1000
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

set cursorline

set notimeout

set hidden
set nobackup
set noswapfile

set autoread

"공백문자들
set list
set listchars=tab:≈.,trail:·,extends:ø,nbsp:·

"자연스러운 분할
set splitbelow          " Horizontal split below current.
set splitright          " Vertical split to right of current.

"original vim only
if !has('nvim')
    set ttyfast
endif

"========================= KEYMAP ==================================
"basic
let mapleader="\<Space>"
imap jj <ESC>
imap ㅓㅓ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>
nmap <silent> <Leader>/ :nohlsearch<CR>
map <silent> <F1> :help quickref<CR>

"paste & delete without yanking
nnoremap <leader>d "_d
nnoremap <leader>dd "_dd
nnoremap <leader>p "0p

"Toggl Paste"
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

"save
nnoremap <Leader>s :w<CR>

map <C-S-n> :NERDTreeToggle<CR>

"buffer move
nmap <silent> <leader>h :BuffergatorMruCyclePrev<cr>
nmap <silent> <leader>l :BuffergatorMruCycleNext<CR>
nmap <silent> <leader>q :bp <bar> bd #<CR>
nmap <silent> <leader>f :b
nmap <silent> <leader>e :CtrlPBuffer<cr>

nmap <silent> <leader>r :CtrlPMRUFiles<cr>

"ternjs
nmap <silent> <leader>td :TernDef<CR>
nmap <silent> <leader>tp :TernDefPreview<CR>

"Dash
nmap <silent> <leader>d <Plug>DashGlobalSearch

"Unite
nmap <silent> <leader>u :Unite<CR>

"Unite-outline
nmap <silent> <leader>o :Unite outline<CR>

"TrimWhiteSpace
nnoremap <silent> <Leader>rts :call TrimWhiteSpace()<CR>

"========================= ETC ==================================
"저장시 필요없는 스페이스 지우기
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

autocmd FileType * autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileType * autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FileType * autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd FileType * autocmd BufWritePre     * :call TrimWhiteSpace()

"모드에따라 커서모양 변경
if has("unix")
  let s:uname = system("uname -s")
  if s:uname == "Darwin\n"
    " OS X iTerm 2 settings
    if exists('$TMUX')
      let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
      let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    else
      let &t_SI = "\<Esc>]50;CursorShape=1\x7"
      let &t_EI = "\<Esc>]50;CursorShape=0\x7"
    endif
  else
    " linux settings (gnome-terminal)
    " TODO: Presently in GNOME3 terminal seems to ignore this gconf setting.
    " Need to open a bug with them...
    if has("autocmd")
      au InsertEnter * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape ibeam"
      au InsertLeave * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape block"
      au VimLeave * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape ibeam"
    endif
  endif
endif

"=========================== MACVIM =========================="
"remove scrollbars"
set guioptions-=r
set guioptions-=r
set guioptions-=l
set guioptions-=L
set guioptions-=b
set guifont=Bitstream\ Vera\ Sans\ Mono:h12

"change shell cause zsh problem"
if has("gui_macvim")
    set shell=/bin/bash\ -l
endif


