filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    "Plugin
    Plugin 'gmarik/Vundle.vim'

    "File/Buffer
    Plugin 'The-NERD-tree'
    Plugin 'ctrlp.vim'
        let g:ctrlp_working_path_mode = 'ra'
        let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist\|lib\|report\|build'

    "Git
    Plugin 'tpope/vim-fugitive'
    Plugin 'airblade/vim-gitgutter'

    "Javascript
    Plugin 'node.js'
    Plugin 'wookiehangover/jshint.vim'
    Plugin 'Enhanced-Javascript-syntax'
    Plugin 'crusoexia/vim-javascript-lib'
    Plugin 'pangloss/vim-javascript'
        let b:javascript_fold = 0
    Plugin 'heavenshell/vim-jsdoc'
    Plugin 'marijnh/tern_for_vim'
        let tern_show_argument_hint='on_move'
        let tern_show_signature_in_pum=1

    "Lint/Autocomplete
    Plugin 'scrooloose/syntastic'
        let g:syntastic_check_on_open=1

    Plugin 'Shougo/neocomplcache.vim'
        let g:neocomplcache_enable_at_startup = 1


    "Colorthemes
    Plugin 'mango.vim'
    Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
    Plugin 'crusoexia/vim-monokai'
        let g:monokai_italic = 1
        let g:monokai_thick_border = 1
        let g:monokai_zentre = 1
    Plugin 'cocopon/iceberg.vim'
    Plugin 'altercation/solarized', {'rtp': 'vim-colors-solarized'}

    "Editing
    Plugin 'tpope/vim-surround'

    "Navigate
    Plugin 'EasyMotion'

    "ETC
    Plugin 'nathanaelkane/vim-indent-guides'
        hi IndentGuidesOdd  ctermbg=black
        hi IndentGuidesEven ctermbg=darkgrey
        let g:indent_guides_start_level = 2
        let g:indent_guides_guide_size = 1
        let g:indent_guides_enable_on_vim_startup = 1
    Plugin 'bling/vim-airline'
call vundle#end()            " required

filetype plugin indent on    " required
syntax on     " 문법 하이라이트 킴"
syntax sync fromstart

set number            "line 표시를 해줍니다.
set cindent            " c style index
set autoindent
set smartindent
set copyindent

set shiftwidth=4      " shift를 4칸으로 ( >, >>, <, << 등의 명령어)
set tabstop=4         " tab을 4칸으로
set expandtab       " tab 대신 띄어쓰기로

set ignorecase      " 검색시 대소문자 구별하지않음
set hlsearch         " 검색시 하이라이트(색상 강조)
set incsearch

set background=dark  " 검정배경을 사용할 때, (이 색상에 맞춰 문법 하이라이트 색상이 달라집니다.)
set nocompatible   " 방향키로 이동가능
set fileencodings=utf-8,euc-kr    " 파일인코딩 형식 지정
set bs=indent,eol,start    " backspace 키 사용 가능
set history=1000    " 명령어에 대한 히스토리를 1000개까지
set undolevels=1000
set ruler              " 상태표시줄에 커서의 위치 표시
set title               " 제목을 표시
set showmatch    " 매칭되는 괄호를 보여줌
set nowrap         " 자동 줄바꿈 하지 않음
set wildmenu           " tab 자동완성시 가능한 목록을 보여줌
set clipboard=unnamed
set cursorline
set foldmethod=indent
set nofoldenable
set t_Co=256
set showcmd
set smarttab

colorscheme iceberg 

set hidden
set nobackup
set noswapfile

set list
set listchars=tab:≈.,trail:¬,extends:ø,nbsp:.,eol:√

"스크롤바들 제거" 
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
set guioptions-=b
set guifont=Bitstream\ Vera\ Sans\ Mono:h12

"키맵"
let mapleader=","
imap jj <ESC>
imap ㅓㅓ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>
nmap <silent> ,/ :nohlsearch<CR>

"플러그인 키맵"
map <D-1> :NERDTreeToggle<CR>
map <D-e> :CtrlPMRUFiles<CR>
map <D-E> :CtrlPBuffer<CR>
cmap nt NERDTreeToggle
cmap cp CtrlP
cmap cpm CtrlPMRUFiles
cmap cpb CtrlPBuffer

"Neocompletion
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplcache#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()
inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

if has("gui_macvim")
    set shell=/bin/bash\ -l
endif

