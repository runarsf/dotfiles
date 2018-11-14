"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Sections:
"    -> General
"    -> Plugins
"    -> VIM user interface
"    -> Status line / Tabs
"    -> Editing / Binds
"    -> Colors
"    -> Misc
"    -> Functions / Utilities
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=500

" Set to auto read when a file is changed from the outside
set autoread

" Disable text wrapping
set nowrap

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W w !sudo tee % > /dev/null

" Enables VI iMproved enhancements
set nocompatible

" Enable syntax highlighting
syntax on
syntax enable

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf-8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
" -> Plugin configs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable filetype plugins
"filetype plugin on
"filetype indent on
filetype off " required for vundle

"Plugin 'octol/vim-cpp-enhanced-highlight'
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin() " vvv PLUGINS vvv
Plugin 'VundleVim/Vundle.vim'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'tpope/vim-eunuch'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'airblade/vim-gitgutter'
Plugin 'junegunn/goyo.vim'
Plugin 'reedes/vim-colors-pencil'
Plugin 'ervandew/supertab'
Plugin 'gbigwood/Clippo'
Plugin 'sjl/badwolf'
Plugin 'vim-scripts/IndentAnything'
Plugin 'wincent/terminus'
call vundle#end() " ^^^ PLUGINS ^^^

filetype plugin indent on " required for vundle

" lightline config
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }

" NERDTree config
" open a NERDTree automatically when vim starts up if no files were specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" open NERDTree automatically when vim starts up on opening a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
" close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" open NERDTree on right
let g:NERDTreeWinPos = "right"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable line numbers and set them to relative
set number
set ruler
set relativenumber

" Set lines to the cursor - when moving vertically
set so=7

" Avoid garbled characters in Chinese language windows OS
let $LANG='en'
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the Wild menu
set wildmenu

" Markdown file interpreting
let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" Add a bit extra margin to the left
"set foldcolumn=1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Status line / Tabs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" Format the status line
" set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

" Status line color
if !has('gui_running')
  set t_Co=256
endif

" Tab settings
set softtabstop=0 noexpandtab
set smarttab

" tab size
set shiftwidth=2
set tabstop=2

" Tab navigation like Firefox.
nnoremap <C-S-tab> :tabprevious<CR>
nnoremap <C-tab>   :tabnext<CR>
nnoremap <C-t>     :tabnew<CR>
inoremap <C-S-tab> <Esc>:tabprevious<CR>i
inoremap <C-tab>   <Esc>:tabnext<CR>i
inoremap <C-t>     <Esc>:tabnew<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing / Binds
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map types
":nmap - Display normal mode maps
":imap - Display insert mode maps
":vmap - Display visual and select mode maps
":smap - Display select mode maps
":xmap - Display visual mode maps
":cmap - Display command-line mode maps
":omap - Display operator pending mode maps

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

" Fast quit
nmap <leader>q :q<cr>

" Toggle Zen mode / Goyo
nmap <leader>z :Goyo<cr>

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Toggle NERDTree
map <C-o> :NERDTreeToggle<CR>

" Splits navigation.
set splitbelow
set splitright
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Get line, word and character counts with F3:
map <F3> :!wc %<CR>

" Spell-check set to F6:
map <F6> :setlocal spell! spelllang=en_us,es<CR>

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Copy selected text to system clipboard (requires gvim installed):
vnoremap <C-c> "*Y :let @+=@*<CR>
map <C-p> "+P

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tab colors
"highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
"highlight TabLine ctermfg=Blue ctermbg=Yellow
"highlight TabLineSel ctermfg=Red ctermbg=Yellow

" Colorscheme
set background=dark
color default

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Disable scrollbars (real hackers don't use scrollbars for navigation!)
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
    set undofile
catch
endtry

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500
" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions-=e
    set t_Co=256
    set guitablabel=%M\ %t
endif

" Readmes autowrap text:
autocmd BufRead,BufNewFile *.md set tw=79

" Automatically deletes all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions / Utilities
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" autoreload .vimrc
augroup myvimrchooks
    au!
    autocmd bufwritepost .vimrc source ~/.vimrc
augroup END

" Switch colorscheme with Goyo
function! s:goyo_enter()
    colorscheme pencil
		set background=dark
endfunction

function! s:goyo_leave()
    colorscheme default
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
