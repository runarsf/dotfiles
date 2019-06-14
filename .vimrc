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
" General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=500

" Set to auto read when a file is changed from the outside
set autoread

" Text wrapping
"set wrap

" Disable wrapping
set textwidth=0
set wrapmargin=0

" :W sudo saves the file
" (useful for handling the permission-denied error)
command! W w !sudo tee % > /dev/null

" Enables VI iMproved enhancements
set nocompatible

" GUI Font
set guifont=Source\ Code\ Pro

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
" -> Plugin manager: Vundle
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"if ! filereadable(expand('~/.vim/bundle/Vundle.vim/autoload/vundle.vim'))
""	echo "Downloading VundleVim/Vundle.Vim to manage plugins..."
"	" Prefix commands with 'silent' to not show output
"	!mkdir -p ~/.vim/bundle/
"	!git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"	autocmd VimEnter * PluginInstall | source ~/.vimrc
"endif
let vundlePreInstalled=1
if !filereadable(expand('~/.vim/bundle/Vundle.vim/README.md'))
	echo "Installing Vundle.."
	echo ""
	silent !mkdir -p ~/.vim/bundle
	silent !git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	let vundlePreInstalled=0
endif

" Filetype plugins
"filetype plugin on
"filetype indent on
filetype off " required for vundle

set rtp+=~/.vim/bundle/Vundle.vim " required for vundle

" All plugins have to be between the vundle begin and end
" Syntax: Plugin 'git_username/git_repo'
call vundle#begin()
" General
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-eunuch'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'itchyny/lightline.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'airblade/vim-gitgutter'
Plugin 'junegunn/goyo.vim'
Plugin 'ervandew/supertab'
Plugin 'gbigwood/Clippo'
Plugin 'vim-scripts/IndentAnything'
Plugin 'justinmk/vim-sneak'
Plugin 'zefei/vim-colortuner'
Plugin 'junegunn/fzf'
Plugin 'osyo-manga/vim-hopping'
Plugin 'ryanoasis/vim-devicons'
Plugin 'lifepillar/vim-cheat40'

" Synax highlighting
Plugin 'vim-scripts/nginx.vim'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'kovetskiy/sxhkd-vim'
Plugin 'baskerville/vim-sxhkdrc'

" Colorschemes
Plugin 'sjl/badwolf'
Plugin 'xero/sourcerer.vim'
Plugin 'AlessandroYorba/Sierra'
Plugin 'altercation/vim-colors-solarized'
Plugin 'morhetz/gruvbox'
call vundle#end()

filetype plugin indent on " required for vundle

" Install plugins if vundle was recently installed, part of Vundle installation
if vundlePreInstalled == 0
	echo "Installing plugins..."
	echo ""
	:PluginInstall
endif

" Lightline config
let g:lightline = { 'colorscheme': 'seoul256' }

" Vim-sneak config
let g:sneak#label = 1

" Sierra config
"let g:sierra_Sunset = 1
"let g:sierra_Twilight = 1
let g:sierra_Midnight = 1
"let g:sierra_Pitch = 1

" Cheat40 config
let g:cheat40_use_default = 1
nmap <F1> :Cheat40<CR>
imap <F1> :Cheat40<CR>

" Gruvbox config
let g:gruvbox_contrast_dark="hard"

" NERDTree config
" Open a NERDTree automatically when vim starts up if no files were specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" Open NERDTree automatically when vim starts up on opening a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
" Close vim if the only window left open is a NERDTree
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Open NERDTree on right
let g:NERDTreeWinPos = "left"
" Toggle NERDTree
map <C-o> :NERDTreeToggle<CR>

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

" Margin to the left
set foldcolumn=0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Status line / Tabs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" Format the status line
"set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

" Status line color
if !has('gui_running')
  set t_Co=256
endif

" Tab settings
set softtabstop=0 noexpandtab
set smarttab

" Tab size
set shiftwidth=4
set tabstop=4

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
"|> Map types
"|=> :nmap - Display normal mode maps
"|=> :imap - Display insert mode maps
"|=> :vmap - Display visual and select mode maps
"|=> :smap - Display select mode maps
"|=> :xmap - Display visual mode maps
"|=> :cmap - Display command-line mode maps
"'=> :omap - Display operator pending mode maps

" With a map leader it's possible to do extra key combinations
" e.g. <leader>w saves the current file
let mapleader = ","

" Run python file
nnoremap <F5> :echo system('python3 "' . expand('%') . '"')<cr>

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
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
"vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Get line, word and character counts with F3:
map <F3> :!wc %<CR>

" Spell-check set to F6:
map <F6> :setlocal spell! spelllang=en_us,es<CR>

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Copy selected text to system clipboard (requires gvim installed):
vnoremap <C-c> "*Y :let @+=@*<CR>
map <C-p> "+P

" LaTeX
" Word count:
autocmd FileType tex map <leader>w :w !detex \| wc -w<CR>
" Code snippets
autocmd FileType tex inoremap ,fr \begin{frame}<Enter>\frametitle{}<Enter><Enter><++><Enter><Enter>\end{frame}<Enter><Enter><++><Esc>6kf}i
autocmd FileType tex inoremap ,fi \begin{fitch}<Enter><Enter>\end{fitch}<Enter><Enter><++><Esc>3kA
autocmd FileType tex inoremap ,exe \begin{exe}<Enter>\ex<Space><Enter>\end{exe}<Enter><Enter><++><Esc>3kA
autocmd FileType tex inoremap ,em \emph{}<++><Esc>T{i
autocmd FileType tex inoremap ,bf \textbf{}<++><Esc>T{i
autocmd FileType tex vnoremap , <ESC>`<i\{<ESC>`>2la}<ESC>?\\{<Enter>a
autocmd FileType tex inoremap ,it \textit{}<++><Esc>T{i
autocmd FileType tex inoremap ,ct \textcite{}<++><Esc>T{i
autocmd FileType tex inoremap ,cp \parencite{}<++><Esc>T{i
autocmd FileType tex inoremap ,glos {\gll<Space><++><Space>\\<Enter><++><Space>\\<Enter>\trans{``<++>''}}<Esc>2k2bcw
autocmd FileType tex inoremap ,x \begin{xlist}<Enter>\ex<Space><Enter>\end{xlist}<Esc>kA<Space>
autocmd FileType tex inoremap ,ol \begin{enumerate}<Enter><Enter>\end{enumerate}<Enter><Enter><++><Esc>3kA\item<Space>
autocmd FileType tex inoremap ,ul \begin{itemize}<Enter><Enter>\end{itemize}<Enter><Enter><++><Esc>3kA\item<Space>
autocmd FileType tex inoremap ,li <Enter>\item<Space>
autocmd FileType tex inoremap ,ref \ref{}<Space><++><Esc>T{i
autocmd FileType tex inoremap ,tab \begin{tabular}<Enter><++><Enter>\end{tabular}<Enter><Enter><++><Esc>4kA{}<Esc>i
autocmd FileType tex inoremap ,ot \begin{tableau}<Enter>\inp{<++>}<Tab>\const{<++>}<Tab><++><Enter><++><Enter>\end{tableau}<Enter><Enter><++><Esc>5kA{}<Esc>i
autocmd FileType tex inoremap ,can \cand{}<Tab><++><Esc>T{i
autocmd FileType tex inoremap ,con \const{}<Tab><++><Esc>T{i
autocmd FileType tex inoremap ,v \vio{}<Tab><++><Esc>T{i
autocmd FileType tex inoremap ,a \href{}{<++>}<Space><++><Esc>2T{i
autocmd FileType tex inoremap ,sc \textsc{}<Space><++><Esc>T{i
autocmd FileType tex inoremap ,chap \chapter{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ,sec \section{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ,ssec \subsection{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ,sssec \subsubsection{}<Enter><Enter><++><Esc>2kf}i
autocmd FileType tex inoremap ,st <Esc>F{i*<Esc>f}i
autocmd FileType tex inoremap ,beg \begin{DELRN}<Enter><++><Enter>\end{DELRN}<Enter><Enter><++><Esc>4k0fR:MultipleCursorsFind<Space>DELRN<Enter>c
autocmd FileType tex inoremap ,up <Esc>/usepackage<Enter>o\usepackage{}<Esc>i
autocmd FileType tex nnoremap ,up /usepackage<Enter>o\usepackage{}<Esc>i
autocmd FileType tex inoremap ,tt \texttt{}<Space><++><Esc>T{i
autocmd FileType tex inoremap ,bt {\blindtext}
autocmd FileType tex inoremap ,nu $\varnothing$
autocmd FileType tex inoremap ,col \begin{columns}[T]<Enter>\begin{column}{.5\textwidth}<Enter><Enter>\end{column}<Enter>\begin{column}{.5\textwidth}<Enter><++><Enter>\end{column}<Enter>\end{columns}<Esc>5kA
autocmd FileType tex inoremap ,rn (\ref{})<++><Esc>F}i

" .bib
autocmd FileType bib inoremap ,a @article{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>journal<Space>=<Space>{<++>},<Enter>volume<Space>=<Space>{<++>},<Enter>pages<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i
autocmd FileType bib inoremap ,b @book{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>6kA,<Esc>i
autocmd FileType bib inoremap ,c @incollection{<Enter>author<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>booktitle<Space>=<Space>{<++>},<Enter>editor<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i

" .py
autocmd FileType python set expandtab
"autocmd FileType python set textwidth=79
autocmd FileType python set tabstop=4
autocmd FileType python set softtabstop=4
autocmd FileType python set shiftwidth=4
autocmd FileType python set autoindent

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tab colors
highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
highlight TabLine ctermfg=Blue ctermbg=Yellow
highlight TabLineSel ctermfg=Red ctermbg=Yellow

" Colorscheme
set background=dark
colorscheme default

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

" Enable mouse support
set mouse=a

" Turn persistent undo on
" Undo even when you close a buffer/VIM
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

" Automatically deletes all trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions / Utilities
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autoreload .vimrc
augroup myvimrchooks
	au!
	autocmd bufwritepost .vimrc source ~/.vimrc
augroup END

" Switch colorscheme with Goyo
function! s:goyo_enter()
	set background=dark
	colorscheme gruvbox
endfunction

function! s:goyo_leave()
	colorscheme default
endfunction

function! s:DiffWithSaved()
	let filetype=&ft
	diffthis
	vnew | r # | normal! 1Gdd
	diffthis
	exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
