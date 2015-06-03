filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    "Plugin
    Plugin 'gmarik/Vundle.vim'

    "File/Buffer
    Plugin 'scrooloose/nerdtree'
    Plugin 'Xuyuanp/nerdtree-git-plugin'
    Plugin 'ctrlp.vim'
        let g:ctrlp_working_path_mode = 'ra'
        let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist\|lib\|report\|build'
        let NERDTreeQuitOnOpen=1
    Plugin 'rking/ag.vim'
    Plugin 'mkitt/tabline.vim'

    "Git
    Plugin 'tpope/vim-fugitive'
    Plugin 'airblade/vim-gitgutter'

    "Javascript
    Plugin 'node.js'
    "Plugin 'Enhanced-Javascript-syntax'
    Plugin 'othree/yajs'
    "Plugin 'crusoexia/vim-javascript-lib'
    "Plugin 'pangloss/vim-javascript'
        "let b:javascript_fold = 0
        "let javascript_ignore_javaScriptdoc = 0
    Plugin 'heavenshell/vim-jsdoc'
    Plugin 'marijnh/tern_for_vim'
        let tern_show_argument_hint='on_move'
        let tern_show_signature_in_pum=1
    Plugin 'javascript-libraries-syntax'
        let g:used_javascript_libs = 'jquery,underscore,jasmine,requirejs'
    Plugin 'juanpabloaj/vim-istanbul'

    "markdown"
    Plugin 'jtratner/vim-flavored-markdown'
    augroup markdown
        au!
        au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
    augroup END

    "Lint/Autocomplete
    Plugin 'scrooloose/syntastic'
        let g:syntastic_check_on_open=1
        let g:syntastic_javascript_checkers=["eslint"]
    Plugin 'Valloric/YouCompleteMe'
        let g:ycm_add_preview_to_completeopt=0
        let g:ycm_confirm_extra_conf=0
        set completeopt-=preview

    "Colorthemes
    Plugin 'mango.vim'
    Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
    Plugin 'crusoexia/vim-monokai'
        let g:monokai_italic = 1
        let g:monokai_thick_border = 1
        let g:monokai_zentre = 1
    Plugin 'cocopon/iceberg.vim'
    Plugin 'altercation/solarized', {'rtp': 'vim-colors-solarized'}
    Plugin 'nanotech/jellybeans.vim'

    "Editing
    Plugin 'tpope/vim-surround'
    Plugin 'scrooloose/nerdcommenter'
    Plugin 'terryma/vim-expand-region'
        vmap <leader>k <Plug>(expand_region_expand)
        vmap <leader>j <Plug>(expand_region_shrink)

    "Navigate
    Plugin 'EasyMotion'

    "ETC
    "Plugin 'nathanaelkane/vim-indent-guides'
    "    hi IndentGuidesOdd  ctermbg=black
    "    hi IndentGuidesEven ctermbg=darkgrey
    "    let g:indent_guides_start_level = 2
    "    let g:indent_guides_guide_size = 1
    "    let g:indent_guides_enable_on_vim_startup = 1
    Plugin 'bling/vim-airline'
    Plugin 'Shougo/unite.vim'
    Plugin 'Shougo/vimproc.vim'
    Plugin 'tpope/vim-obsession'
    Plugin 'rizzatti/dash.vim'
call vundle#end()            " required

filetype plugin indent on    " required


"문법 하이라이트"
syntax on
syntax sync fromstart

set t_Co=256
set lazyredraw
set ttyfast
"검정배경을 사용할 때, (이 색상에 맞춰 문법 하이라이트 색상이 달라집니다.)
set background=dark
colorscheme jellybeans

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

"공백문자들
set list
set listchars=tab:≈.,trail:·,extends:ø,nbsp:·


"키맵"
let mapleader="\<Space>"
imap jj <ESC>
imap ㅓㅓ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>
nmap <silent> ,/ :nohlsearch<CR>

"플러그인 키맵"
map <C-S-n> :NERDTreeToggle<CR>

"저장시 필요없는 스페이스 지우기"
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

nnoremap <silent> <Leader>rts :call TrimWhiteSpace()<CR>
autocmd FileType javascript autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileType javascript autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FileType javascript autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd FileType javascript autocmd BufWritePre     * :call TrimWhiteSpace()

"Toggl Paste"
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

"delete/paste without yanking
nnoremap <leader>d "_d
vnoremap <leader>p "_dP

"save
nnoremap <Leader>w :w<CR>

"for macvim"
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

