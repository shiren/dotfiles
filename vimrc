set background=dark

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

"=== KEYMAP ==="
"basic
let mapleader = " "
imap jk <ESC>
imap ㅓㅏ <ESC>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>

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
