filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

    Plugin 'gmarik/Vundle.vim'
    Plugin 'tpope/vim-fugitive'
    Plugin 'L9'
    Plugin 'git://git.wincent.com/command-t.git'
    Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
   
    Plugin 'The-NERD-tree'
    Plugin 'EasyMotion'
    Plugin 'ctrlp.vim'
    let g:ctrlp_working_path_mode = 'ra'
    let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist\|lib\|report\|build'

    Plugin 'node.js'
    Plugin 'wookiehangover/jshint.vim'
    Plugin 'Enhanced-Javascript-syntax'
    Plugin 'crusoexia/vim-javascript-lib'
    Plugin 'pangloss/vim-javascript'
    let b:javascript_fold = 0

    Plugin 'nathanaelkane/vim-indent-guides'
    hi IndentGuidesOdd  ctermbg=black
    hi IndentGuidesEven ctermbg=darkgrey
    let g:indent_guides_start_level = 2
    let g:indent_guides_guide_size = 1
    let g:indent_guides_enable_on_vim_startup = 1

    Plugin 'scrooloose/syntastic'
    " This does what it says on the tin. It will check your file on open too,
    " not just on save.
    " " You might not want this, so just leave it out if you don't.
    let g:syntastic_check_on_open=1

    Plugin 'Valloric/YouCompleteMe'
    " These are the tweaks I apply to YCM's config, you don't need them but
    " they might help.
    " " YCM gives you popups and splits by default that some people might not
    " like, so these should tidy it up a bit for you.
    let g:ycm_add_preview_to_completeopt=0
    let g:ycm_confirm_extra_conf=0
    set completeopt-=preview
   
    Plugin 'marijnh/tern_for_vim'
    let tern_show_argument_hint='on_move'
    let tern_show_signature_in_pum=1
    
    Plugin 'bling/vim-airline'
 
    "Plugin 'mango.vim'
    "Plugin 'chriskempson/tomorrow-theme', {'rtp': 'vim/'}
    Plugin 'crusoexia/vim-monokai'
    let g:monokai_italic = 1
    let g:monokai_thick_border = 1
    let g:monokai_zentre = 1
    
call vundle#end()            " required
filetype plugin indent on    " required
syntax on     " 문법 하이라이트 킴"

set number            "line 표시를 해줍니다.
set ai                    " auto index
set si                    " smart index
set cindent            " c style index
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
set ruler              " 상태표시줄에 커서의 위치 표시
set nobackup      " 백업파일을 만들지 않음
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

"스크롤바들 제거"   
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
set guioptions-=b
set guifont=Bitstream\ Vera\ Sans\ Mono:h12

"키맵"
map <D-1> :NERDTreeToggle<CR>

"vim시작시 열파일없으면 NERDTree실행"
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

colorscheme monokai 

