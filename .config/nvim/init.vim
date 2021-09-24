" vim: set foldmethod=marker foldlevel=0 nomodeline:
" Neat minimal things: https://github.com/nickjj/dotfiles/blob/master/.vimrc
let mapleader = ','
let maplocalleader = ','

" Auto install vim-plug {{{
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" Automatically calls `filetype plugin indent on` and `syntax enable`
silent! if plug#begin(stdpath('data') . '/plugged') " {{{
Plug 'tpope/vim-vinegar' " netrw {{{
  "let loaded_netrwPlugin = 0 " netrw version, 0 to disable
  let g:netrw_banner = 0
  let g:netrw_liststyle = 3 " 1 or 3
  let g:netrw_browse_split = 4 " 1
  let g:netrw_altv = 1
  let g:netrw_winsize = 25
  let g:netrw_keepdir = 0
  let g:netrw_sort_options = 'i'
  let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+,\(^\|\s\s\)ntuser\.\S\+'
  let g:netrw_sort_sequence = '[\/]$,*'
  let g:netrw_list_hide = '.*.swp$,
                        \ *.pyc$,
                        \ *.log$,
                        \ *.o$,
                        \ *.xmi$,
                        \ *.swp$,
                        \ *.bak$,
                        \ *.pyc$,
                        \ *.class$,
                        \ *.jar$,
                        \ *.war$,
                        \ *.png$,
                        \ *.jpg$,
                        \ *.mkv$,
                        \ *.mp4$,
                        \ *.mp3$,
                        \ *node_modules*,
                        \ *__pycache__*'

  augroup ProjectDrawer | autocmd!
    "autocmd VimEnter * silent Vexplore | wincmd p
    "autocmd FileType netrw set nolist
    " No argument was specified
    autocmd VimEnter * if !argc() && !exists("s:std_in") | silent Lexplore | wincmd p | endif
    autocmd StdinReadPre * let s:std_in=1
    " Specified argument is a directory
    autocmd VimEnter * if isdirectory(expand('<afile>')) && !exists("s:std_in") | silent vnew | endif
    " Only window left
    autocmd BufEnter * if (winnr("$") == 1 && getbufvar(winbufnr(winnr()), "&filetype") == "netrw") | q | endif
    autocmd FileType netrw setlocal bufhidden=wipe
    autocmd FileType netrw vertical resize 25
    autocmd FileType netrw nnoremap <buffer> q :q<CR>
  augroup END

  "map <silent> <C-n> :NetrwToggle <bar> wincmd p<CR>
  map <silent> <C-n> :Lexplore<CR>
" }}}
Plug 'nathanaelkane/vim-indent-guides' " {{{
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 1
let g:indent_guides_auto_colors = 0
"let g:indent_guides_guide_size = 2
let g:indent_guides_color_change_percent = 10
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=gray ctermbg=236
" vim-one
" autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#363840 ctermbg=237
" gruvbox
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd guibg=#282828 ctermbg=237
" }}}

" Syntax highlighting {{{
  Plug 'chr4/nginx.vim', { 'for': 'nginx' }
  Plug 'storyn26383/vim-vue', { 'for': 'vue' }
  Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
  Plug 'kovetskiy/sxhkd-vim', { 'for': 'sxhkdrc' }
" }}}

"Plug 'rakr/vim-one' | let g:one_allow_italics = 0
Plug 'gruvbox-community/gruvbox' " {{{
  if !exists('g:gruvbox_contrast_light')
    let g:gruvbox_contrast_light='hard'
  endif
" }}}

let g:colorscheme = 'gruvbox'
call plug#end() | endif " }}}

if (has('termguicolors'))
  set termguicolors
endif

" Enable 256-colors, has to be set before colorscheme
set t_Co=256
set t_AB=^[[48;5;%dm
set t_AF=^[[38;5;%dm

" Specific colorscheme settings (must come after setting your colorscheme).
function! CustomHighlights() abort
  if (g:colors_name == 'gruvbox')
    if (&background == 'dark')
      hi Visual cterm=NONE ctermfg=NONE ctermbg=237 guibg=#3a3a3a
    else
      hi Visual cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
      hi CursorLine cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
      hi ColorColumn cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
    endif
  endif
  "highlight Comment        cterm=italic
  "highlight Normal         ctermbg=NONE guibg=NONE
  "highlight LineNr         ctermbg=NONE guibg=NONE
  "highlight SignColumn     ctermbg=NONE guibg=NONE
  "highlight BufferLineFill guibg=NONE
  "highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
  "highlight TabLine ctermfg=Blue ctermbg=Yellow
  "highlight TabLineSel ctermfg=Red ctermbg=Yellow
endfunction

augroup MyColors | autocmd!
  autocmd ColorScheme * call CustomHighlights()
augroup END

set background=dark
execute "colorscheme " . g:colorscheme

if !exists("g:syntax_on") | syntax enable | endif
set regexpengine=1
set clipboard+=unnamedplus
set history=500
set autoread
set modeline modelines=5
set ffs=unix,dos,mac
set nobackup nowritebackup noswapfile
set splitbelow splitright
set mouse=c
set noerrorbells novisualbell t_vb=
set conceallevel=0
set synmaxcol=1000
set scrolljump=0
set scrolloff=7
set nocursorline nocursorcolumn
set number relativenumber
set showcmd
set showfulltag
set showmatch
set langmenu=en
set wildmenu
set wildmode=longest:full,full
set path+=**
set wildignore+=**/node_modules/**
set hidden
set backspace=indent,eol,start
set whichwrap+=<,>,h,l
set ignorecase smartcase
set lazyredraw
set magic
set incsearch
set noshowmatch
set hlsearch
set mat=2
set foldcolumn=0 numberwidth=1
set updatetime=300
set shortmess+=cI
set nospell
set signcolumn=no
set list listchars=trail:·,nbsp:⎵,tab:┊» " ¦┆┊ eol:⏎ (          )
set foldmethod=marker foldmarker={{{,}}}

" Tabs and lines {{{
set smarttab        " Enabling this will make the tab key
                    " (in insert mode) insert spaces or
                    " tabs to go to the next indent of the
                    " next tabstop when the cursor is at
                    " the beginning of a line
                    " (i.e. the only preceding characters
                    " are whitespace).
set tabstop=2       " The width of a TAB is set to 2.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 2.
set shiftwidth=2    " Indents will have a width of 2
set softtabstop=0   " Sets the number of columns for a TAB
set expandtab       " Expand TABs to spaces

set nowrap
set textwidth=0
set wrapmargin=0

" Time for key combinations to time out, like <leader>w in Normal mode.
set ttimeout
set ttimeoutlen=50 " affect Escape from insert mode delay
set timeoutlen=600

augroup timeout | autocmd!
  autocmd InsertEnter * set timeoutlen=300
  autocmd InsertLeave * set timeoutlen=600
augroup END

if exists('g:gui_oni')
  set noswapfile
  set smartcase
  set mouse=a
  set noshowmode
  set noruler
  set laststatus=0
  set noshowcmd
endif

source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Use ctrl-[hjkl] to select the active split!
" https://vim.fandom.com/wiki/Switch_between_Vim_window_splits_easily
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Tab navigation
nnoremap <C-t> :tabnew<CR>
nnoremap H gT
nnoremap L gt

" Disable CTRL-A on tmux and screen
if $TERM =~ 'screen'
  nnoremap <C-a> <nop>
  nnoremap <leader><C-a> <C-a>
endif

nnoremap <silent> <leader>.. :messages<CR>
nnoremap <silent> <leader>l :set cursorline!<CR>
nnoremap <silent> <leader>ll :set cursorcolumn!<CR>
nnoremap <silent> <leader><space> :nohlsearch<CR>
nnoremap <leader>cfg :e $MYVIMRC<CR>
nnoremap <leader>w :w!<CR>
nnoremap <leader>wq :wq<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>Q :q!<CR>

" Move a line of text using ALT+[jk] or Command+[jk] on mac
"nmap <silent> <M-j> mz:m+<cr>`z
"nmap <silent> <M-k> mz:m-2<cr>`z
"vmap <silent> <M-j> :m'>+<cr>`<my`>mzgv`yo`z
"vmap <silent> <M-k> :m'<-2<cr>`>my`<mzgv`yo`zs

" Moving lines around in all modes
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
inoremap <C-j> <esc>:m .+1<CR>==
inoremap <C-k> <esc>:m .-2<CR>==
nnoremap <leader>j :m .+1<CR>==
nnoremap <leader>k :m .-2<CR>==

imap #dn >/dev/null<space>2>&1
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)

set pastetoggle=<leader>p

" Keep cursor centered when jumping around
nnoremap n nzzzv
nnoremap N Nzzv
nnoremap J mzJ`z
nnoremap <C-j> :cnext<CR>zzzv

" don't undo entire chunk
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" when jump lines is more than 5 add a jump point
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'k'

" Return to last edit position when opening files {{{
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" }}}

" Save and restore code folding {{{
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent! loadview
" }}}

" Disable automatic commenting on newline {{{
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" }}}

" Persistent undo {{{
if has("persistent_undo")
  set undodir=stdpath('data').'/temp_dirs/undodir'
  set undofile
endif
" }}}

" Statusline {{{
set laststatus=2
set cmdheight=1

"highlight SpellHighlight guibg=SeaGreen guifg=003366

set statusline=
set statusline+=%F\ %y\ %m\  " path filetype modified

set statusline+=%=  " right-align from now on
set statusline+=\[%{&fileformat}:%{&fileencoding?&fileencoding:&encoding}\]\
"set statusline+=%{&readonly?'[ro]\ ':''}
set statusline+=%{&mouse=='a'?'[m]\ ':''}
set statusline+=%{&paste?'[p]\ ':''}
set statusline+=%{&wrap=='1'?'[wr]\ ':''}
"set statusline+=%#SpellHighlight#%{&spell?'[sp]':''}%*\
set statusline+=%{&spell?'[sp]\ ':''}
"set statusline+=\[%{mode()}\]\
set statusline+=%v\:%l\/%L\ %{winnr()}  " column:row/rows window
" }}}

" Autoreload .vimrc {{{
augroup myvimrchooks | autocmd!
  autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
augroup END
" }}}

function! ToggleNumbers() " {{{
  " This is based, but defaults have to be set if reading the vimrc isn't possible(?) {{{
  if !exists("b:default_signcolumn")
    let b:default_signcolumn = "no"
  endif
  if !exists("b:default_number")
    let b:default_number = 1
  endif
  if !exists("b:default_relativenumber")
    let b:default_relativenumber = 1
  endif
  " }}}

  if &number || &relativenumber
    let b:default_number = &number
    let b:default_relativenumber = &relativenumber
    let b:default_signcolumn = &signcolumn
    set nonumber
    set list!
    set norelativenumber
    set signcolumn=no
  else
    if b:default_number | set number | endif
    if b:default_relativenumber | set relativenumber | endif
    set list
    execute 'set signcolumn=' . b:default_signcolumn
  endif
endfunction
nmap <silent> <leader>n :call ToggleNumbers()<CR>
" }}}

function! ToggleMouse() " {{{
  if &mouse == 'a'
    set mouse=c
  else
    set mouse=a
  endif
endfunction
nmap <silent> <leader>m :call ToggleMouse()<CR>
" }}}

function! FoldText() " {{{
  set fillchars=fold:\ "
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '{' . printf("%10s", lines_count . ' lines') . ' }'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('»' . repeat(foldchar, v:foldlevel-1) . line, 0, (winwidth(0)*2)/3)
  "let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set foldtext=FoldText()
" }}}
