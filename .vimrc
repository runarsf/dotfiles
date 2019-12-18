" vim: set foldmethod=marker foldlevel=0 nomodeline:
" runarsf's .vimrc {{{
" =========================
"  vim scp://root@domain.tld//home/root/.vimrc
"  vim scp://root@domain.tld/.vimrc
"
"  vim ~/.ssh/config
"  <<host shortname
"  <<  User root
"  <<  Hostname domain.tld
"  <<  Port 22
"  vim scp://shortname/.vimrc
"
"  zo  Open a fold at cursor position.
"  zO  Open all fold at cursor position.
"  zc  Close a fold at cursor position.
"  zm  Increase foldlevel by 1.
"  zM  Close all folds.
"  zr  Decrease foldlevel by 1.
"  zR  Decrease foldlevel to 0; all folds will open.
"
"  h   Left
"  j   Down
"  k   Up
"  l   Right
"
"  K  - Open help page for keyword under cursor.
"  /  - Search for text in current file.
"  :  - Prefix for executing commands.
"
"  :%s/foo/bar/g       Change "foo" to "bar".
"  :s/foo/bar/g        Change "foo" to "bar" on the current line.
"  :%s/foo/bar/gc      Change "foo" to "bar", but ask for confirmation.
"  :%s/\<foo\>/bar/gc  Change whole words matching "foo" to "bar".
"  :%s/foo/bar/gci     Change "foo" to "bar", case sensitive.
"
" }}}======================
" Plugins {{{
" =========================
" TODO: Automatically set up nvim config files to point at vim files
if empty(glob('~/.vim/autoload/plug.vim')) && (has('unix') || has('win32unix'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

if empty(glob('~\vimfiles\autoload\plug.vim')) && empty(glob('~\AppData\Local\nvim\autoload\plug.vim')) && has('win32')
  if has('nvim')
    md ~\AppData\Local\nvim\autoload
    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    (New-Object Net.WebClient).DownloadFile(
      $uri,
      $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(
        "~\AppData\Local\nvim\autoload\plug.vim"
      )
    )
  else
    md ~\vimfiles\autoload
    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    (New-Object Net.WebClient).DownloadFile(
      $uri,
      $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(
        "~\vimfiles\autoload\plug.vim"
      )
    )
  endif
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

silent! if plug#begin('~/.vim/plugged')
" General
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-eunuch'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/goyo.vim'
Plug 'justinmk/vim-sneak'
Plug 'zefei/vim-colortuner'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'osyo-manga/vim-hopping'
Plug 'ryanoasis/vim-devicons'
Plug 'lifepillar/vim-cheat40'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'junegunn/limelight.vim'
Plug 'mechatroner/rainbow_csv'
Plug 'sheerun/vim-polyglot'
Plug 'yuttie/comfortable-motion.vim'
Plug 'mbbill/undotree'
Plug 'luochen1990/rainbow'
Plug 'tyru/open-browser.vim', {'on': 'RunningX'}
Plug 'danro/rename.vim'
Plug 'junegunn/vim-easy-align'
Plug 'terryma/vim-multiple-cursors'
Plug 'vim-scripts/loremipsum'
Plug 'robcsi/viewmaps.vim'
Plug 'tpope/vim-fugitive'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-surround'
if has('python3')
  Plug 'Shougo/denite.nvim'
endif
if &rtp =~ 'denite.nvim'
  Plug 'Shougo/vimfiler.vim'
endif
let node_ver = system('node -v') " either check for !v:shell_error or if node_ver starts with v{num}
if !v:shell_error && v:version >= 703 " FIXME: Find out how to check for has('node')
  Plug 'neoclide/coc.nvim', {'branch': 'release'} " 'do': { -> coc#util#install() }}
endif
"let node_ver = system('node -v') " either check for !v:shell_error or if node_ver starts with v{num}
"if !v:shell_error && has('nvim') && !empty($DISPLAY)
"  Plug 'aurieh/discord.nvim', { 'do': ':UpdateRemotePlugins'}
"endif
"Plug 'unblevable/quick-scope'
"Plug 'AshleyF/VimSpeak'
"Plug 'fmoralesc/vim-pad'
"Plug 'metakirby5/codi.vim'
"Plug 'myusuf3/numbers.vim'
"Plug 'psliwka/vim-smoothie'
"Plug 'thaerkh/vim-workspace'
"Plug 'tpope/vim-sensible'
"Plug 'prabirshrestha/async.vim'
"Plug 'christoomey/vim-tmux-navigator'
"Plug 'davidhalter/jedi-vim'
"Plug 'gbigwood/Clippo'
"Plug 'vim-scripts/IndentAnything'
"Plug 'junegunn/vim-github-dashboard'
"Plug 'junegunn/vim-emoji'
"Plug 'vimwiki/vimwiki'
"Plug 'dbmrq/vim-redacted'
"Plug 'vim-scripts/mru.vim'
"Plug 'tpope/vim-commentary'
"Plug 'kshenoy/vim-origami'
"Plug 'terryma/vim-multiple-cursors'
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'ervandew/supertab'
"Plug 'FredKSchott/CoVim' " pip install twisted argparse service_identity
"Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
"Plug 'junegunn/vim-journal'
"Plug 'dixonary/vimty' " :source vimty.vim

" Syntax highlighting
Plug 'ObserverOfTime/coloresque.vim'
Plug 'chr4/nginx.vim'
Plug 'storyn26383/vim-vue'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'ekalinin/Dockerfile.vim'
Plug 'kovetskiy/sxhkd-vim'
Plug 'baskerville/vim-sxhkdrc'
Plug 'dense-analysis/ale'
Plug 'mboughaba/i3config.vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'rodjek/vim-puppet'
Plug 'nono/jquery.vim'
if v:version >= 703
  Plug 'scrooloose/syntastic'
endif

" Colorschemes
Plug 'tomasr/molokai'
Plug 'AlessandroYorba/Despacio'
Plug 'nightsense/cosmic_latte'
Plug 'nightsense/snow'
Plug 'nightsense/stellarized'
Plug 'junegunn/seoul256.vim'
Plug 'sjl/badwolf'
Plug 'xero/sourcerer.vim'
Plug 'AlessandroYorba/Sierra'
Plug 'mhartington/oceanic-next'
Plug 'rakr/vim-one'
Plug 'liuchengxu/space-vim-dark'
Plug 'jacoborus/tender.vim'
"Plug 'morhetz/gruvbox'
call plug#end()
endif

filetype plugin indent on

" -------------------------
" deoplete.nvim
" -------------------------
"let g:deoplete#enable_at_startup = 1

" -------------------------
" limelight.vim
" -------------------------
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
" Color name (:help gui-colors) or RGB color
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'
" Default: 0.5
"let g:limelight_default_coefficient = 0.7
" Number of preceding/following paragraphs to include (default: 0)
"let g:limelight_paragraph_span = 1
" Beginning/end of paragraph
"   When there's no empty line between the paragraphs
"   and each paragraph starts with indentation
"let g:limelight_bop = '^\s'
"let g:limelight_eop = '\ze\n^\s'
" Highlighting priority (default: 10)
"   Set it to -1 not to overrule hlsearch
"let g:limelight_priority = 10

" -------------------------
" vim-github-dashboard
" -------------------------
"let g:github_dashboard = { 'username': 'runarsf' }

" -------------------------
" vim-airline
" -------------------------
"let g:airline_theme='raven'
"let g:airline#extensions#ale#enabled = 1

" -------------------------
" vim-sneak
" -------------------------
let g:sneak#label = 1

" -------------------------
" vim-indent-guides
" -------------------------
let g:indent_guides_enable_on_vim_startup = 0

" -------------------------
" vim-emoji
" -------------------------
"command! -range EmojiReplace <line1>,<line2>s/:\([^:]\+\):/\=emoji#for(submatch(1), submatch(0))/g

" -------------------------
" Sierra
" -------------------------
"let g:sierra_Sunset = 1
"let g:sierra_Twilight = 1
let g:sierra_Midnight = 1
"let g:sierra_Pitch = 1

" -------------------------
" vim-easy-align
" -------------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" -------------------------
" vim-cheat40
" -------------------------
let g:cheat40_use_default = 1
"nmap <F1> :Cheat40<CR>
"imap <F1> :Cheat40<CR>
function! Cheat()
  if (g:loaded_cheatsheet)
    Cheat40
  endif
endfunction
nnoremap <F1> :call Cheat()<CR>

" -------------------------
" gruvbox
" -------------------------
"let g:gruvbox_contrast_dark="hard"

" -------------------------
" nerdtree
" -------------------------
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

" -------------------------
" coc.nvim
" -------------------------
"let g:coc_force_debug = 1
"set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')} " Add status line support, for integration with other plugin, checkout `:h coc-status`
" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR><Paste>

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <c-space> coc#refresh() " Use <c-space> to trigger completion.

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" }}}======================
" General {{{
" =========================
"set textwidth=80                                             " Make it obvious where 80 characters is
"set colorcolumn=+1
"set nojoinspaces                                             " Use one space, not two, after punctuation.
set history=50                                                " Sets how many lines of history VIM has to remember
set autoread                                                  " Set to auto read when a file is changed from the outside
set clipboard+=unnamedplus
"set wrap                                                     " Enables wrapping
set textwidth=0                                               " Disable wrapping
set wrapmargin=0
set nowrap
command! W w !sudo tee % > /dev/null " :W sudo saves the file
"set foldlevelstart=99                                        " Start with fold level 99 at launch (all folds closed)
set foldmethod=syntax
if expand('%:t') == '.vimrc' | set foldmethod=marker | else | set foldmethod=syntax | endif
set foldlevel=0
set modelines=0                                               " Disable modelines as a security precaution<Paste>
set nomodeline
set nocompatible                                              " Enables VI iMproved enhancements
set guifont=Source\ Code\ Pro                                 " GUI Font
syntax on                                                     " Enable syntax highlighting
syntax enable
set encoding=utf-8                                            " Set utf-8 as standard encoding
set ffs=unix,dos,mac                                          " Use Unix as the standard file type
set nobackup                                                  " Turn backup off, since most stuff is in git
set nowritebackup
set noswapfile
"set complete-=i                                              " Enable completions
set mouse=c                                                   " a, disable mouse support
set noerrorbells                                              " No annoying sound on errors
set novisualbell
set t_vb=
set tm=500
"set cursorline                                               " Highlight current line
"set number                                                   " Enable line numbers
"set relativenumber                                            " Set line numbers to relative
set ruler
set showcmd                                                   " Display incomplete commands
set so=7                                                      " Set lines to the cursor - when moving vertically
"set numberwidth=8                                            " Left margin
let $LANG='en'                                                " Avoid garbled characters in Chinese language in Windows
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
set wildmenu                                                  " Turn on the Wild menu for cycling through command options
set wildmode=longest:full,full                                " longest:list,full
set cmdheight=2                                               " Height of the command bar
set hidden                                                    " A buffer becomes hidden when it is abandoned, recommended for coc
set backspace=eol,start,indent                                " Configure backspace so it acts as it should act
set whichwrap+=<,>,h,l
set ignorecase                                                " Make search case insensitive
set smartcase                                                 " When searching try to be smart about cases
set hlsearch                                                  " Highlight search results
set incsearch                                                 " Makes search act like search in modern browsers
set lazyredraw                                                " Don't redraw while executing macros (good performance config)
set magic                                                     " For regular expressions turn magic on
set showmatch                                                 " Show matching brackets when text indicator is over them
set mat=2                                                     " How many tenths of a second to blink when matching brackets
set foldcolumn=0                                              " Left margin
set updatetime=300                                            " Default 4000
set shortmess+=c                                              " don't give |ins-completion-menu| messages.
"set signcolumn=yes                                           " always show signcolumns

" }}}======================
" A E S T H E T I C S {{{
" =========================
" Tab colors
highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
highlight TabLine ctermfg=Blue ctermbg=Yellow
highlight TabLineSel ctermfg=Red ctermbg=Yellow

" Enable termguicolors
"if (has("termguicolors"))
"  set termguicolors
"endif

" Colorscheme
colorscheme space-vim-dark
set background=dark

highlight Comment cterm=italic
highlight Normal     ctermbg=NONE guibg=NONE
"highlight LineNr     ctermbg=NONE guibg=NONE
"highlight SignColumn ctermbg=NONE guibg=NONE

"if has('nvim')
"  " https://github.com/neovim/neovim/issues/2897#issuecomment-115464516
"  let g:terminal_color_0 = '#4e4e4e'
"  let g:terminal_color_1 = '#d68787'
"  let g:terminal_color_2 = '#5f865f'
"  let g:terminal_color_3 = '#d8af5f'
"  let g:terminal_color_4 = '#85add4'
"  let g:terminal_color_5 = '#d7afaf'
"  let g:terminal_color_6 = '#87afaf'
"  let g:terminal_color_7 = '#d0d0d0'
"  let g:terminal_color_8 = '#626262'
"  let g:terminal_color_9 = '#d75f87'
"  let g:terminal_color_10 = '#87af87'
"  let g:terminal_color_11 = '#ffd787'
"  let g:terminal_color_12 = '#add4fb'
"  let g:terminal_color_13 = '#ffafaf'
"  let g:terminal_color_14 = '#87d7d7'
"  let g:terminal_color_15 = '#e4e4e4'

"  autocmd BufReadPost *
"    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
"    \   exe "normal! g`\"" |
"    \ endif
"endif

" }}}======================
" Status line / Tabs {{{
" =========================
" Always show the status line
set laststatus=2

" Format the status line
"set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
"set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ CWD:\ %r%{getcwd()}%h\ \ Line:\ %l\ \ Column:\ %c

function! s:statusline_expr()
  let pst = "%{&paste ? '[P] ' : ''}"
  let mse = "%{&mouse == 'a' ? '[M] ' : ''}"
  let mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
  let ro  = "%{&readonly ? '[RO] ' : ''}"
  let ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"
  let fug = "%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
  let sep = ' %= '
  let pos = ' %-12(%l : %c%V%) '
  let pct = ' %P'

  return '[%n] %F %<'.pst.mse.mod.ro.ft.fug.sep.pos.'%*'.pct
endfunction
let &statusline = s:statusline_expr()

" Tab settings
set softtabstop=0
set expandtab " expand tabs to spaces (opposite of noexpandtab)
set smarttab
" set nosmarttab
" autocmd FileType python set expandtab
" autocmd FileType python set textwidth=79
" autocmd FileType python set tabstop=4
" autocmd FileType python set softtabstop=4
" autocmd FileType python set shiftwidth=4
" autocmd FileType python set autoindent

" Tab size
set shiftwidth=2
set tabstop=2

" Display whitespace characters
set list
set listchars=trail:·,nbsp:⎵,tab:┊» " ¦┆┊ eol:⏎ (		)
"set fillchars=vert:\|,fold:-

" Tab navigation
nnoremap H gT
nnoremap L gt

nnoremap <C-S-tab> :tabprevious<CR>
nnoremap <C-tab>   :tabnext<CR>
nnoremap <C-t>     :tabnew<CR>
inoremap <C-S-tab> <Esc>:tabprevious<CR>i
inoremap <C-tab>   <Esc>:tabnext<CR>i
inoremap <C-t>     <Esc>:tabnew<CR>

nmap <leader>1 :tabfirst<cr>
nmap <leader>2 2gt
nmap <leader>3 3gt
nmap <leader>4 4gt
nmap <leader>5 5gt
nmap <leader>6 6gt
nmap <leader>7 7gt
nmap <leader>8 8gt
nmap <leader>9 9gt
nmap <leader>0 :tablast<cr>

" }}}======================
" Editing / Binds {{{
" =========================
" Map types:
"  :nmap - Display normal mode maps
"  :imap - Display insert mode maps
"  :vmap - Display visual and select mode maps
"  :smap - Display select mode maps
"  :xmap - Display visual mode maps
"  :cmap - Display command-line mode maps
"  :omap - Display operator pending mode maps

" With a map leader it's possible to do extra key combinations
" e.g. <leader>w saves the current file
let mapleader = ','
let maplocalleader = ','

" Disable CTRL-A on tmux or on screen
if $TERM =~ 'screen'
  nnoremap <C-a> <nop>
  nnoremap <leader><C-a> <C-a>
endif

" esc in insert mode, consider using kj instead, as it's no-op (up-down)
inoremap jk <esc>
" esc in command mode
cnoremap jk <C-C>
" Note: In command mode mappings to esc run the command for some odd
" historical vi compatibility reason. We use the alternate method of
" existing which is Ctrl-C

" qq to record, Q to replay
nnoremap Q @q

" Run python file
nnoremap <F5> :echo system('python3 "' . expand('%') . '"')<cr>

" Toggle paste
"nnoremap <leader>p :set invpaste<CR>
" Breaks if '<leader>p' is in the pasted string
set pastetoggle=<leader>p

" Rebind CapsLock to Escape in X-Sessions
if has('unix') && !empty($DISPLAY)
  au VimEnter * silent! !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
  au VimLeave * silent! !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Caps_Lock'
endif

" Toggle mouse
function! ToggleMouse()
    if &mouse == 'a'
        set mouse=c
    else
        set mouse=a
    endif
endfunc
nmap <silent> <leader>m :call ToggleMouse()<CR>

nnoremap <silent> <leader><space> :nohlsearch<CR>

" Fast config edit
nmap <leader>cfg :e ~/.vimrc<cr>

" Fast saving
nmap <leader>w :w!<cr>

" Fast quit
nmap <leader>q :q<cr>
nmap <leader>Q :q!<cr>

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

" Completions
"inoremap <silent> ,f <C-x><C-f>
"inoremap <silent> ,i <C-x><C-i>
"inoremap <silent> ,l <C-x><C-l>
"inoremap <silent> ,n <C-x><C-n>
"inoremap <silent> ,o <C-x><C-o>
"inoremap <silent> ,t <C-x><C-]>
"inoremap <silent> ,u <C-x><C-u>

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
"vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" Get line, word and character counts with F3:
map <F3> :!wc %<CR>

" Spell-check set to F6:
map <F6> :setlocal spell! spelllang=en_us,no<CR>

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Remove the Windows ^M - when the encodings gets messed up
"noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Copy selected text to system clipboard (requires gvim installed):
vnoremap <C-c> "*Y :let @+=@*<CR>
map <C-p> "+P

" }}}======================
" Misc {{{
" =========================
" Space space goto
"inoremap <Space><Space> <Esc>/<++><Enter>"_c4l

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" #!! shebang
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)

" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T
  set guioptions-=e
  set guitablabel=%M\ %t
  set guioptions-=r                " Disable scrollbars
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L
else
  set t_Co=256
endif

" Markdown file interpreting
let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile .{jscs,jshint,eslint}rc set filetype=json
autocmd BufRead,BufNewFile *.zsh-theme,aliases.local,zshrc.local,*/zsh/configs/* set filetype=zsh
autocmd BufRead,BufNewFile gitconfig.local set filetype=gitconfig
autocmd BufRead,BufNewFile tmux.conf.local set filetype=tmux
autocmd BufRead,BufNewFile vimrc.local set filetype=vim

" Turn persistent undo on
" Undo even when you close a buffer/VIM
try
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
catch
endtry

" Automatically deletes all trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" Save and restore code folding
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent! loadview

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" }}}======================
" Functions / Utilities {{{
" =========================
" Autoreload .vimrc
augroup myvimrchooks
  au!
  autocmd bufwritepost .vimrc source ~/.vimrc
augroup END

" Toggle line numbers
function! ToggleNumbers()
  if &number || &relativenumber
    call EnterInsert()
    set nonumber
    set norelativenumber
  else
    call LeaveInsert()
  endif
endfunction
nmap <silent> <leader>n :call ToggleNumbers()<CR>

function! EnterInsert()
  GitGutterDisable
  set cursorline
  set norelativenumber
  set number
endfunction
function! LeaveInsert()
  GitGutterEnable
  set nocursorline
  set relativenumber
  set number
endfunction
autocmd InsertEnter * call EnterInsert()
autocmd FocusLost * call EnterInsert()
autocmd InsertLeave * call LeaveInsert()
autocmd FocusGained * call LeaveInsert()
autocmd VimEnter * call LeaveInsert()

" Switch colorscheme and enable limelight with Goyo
function! s:goyo_enter()
  colorscheme sierra
  Limelight
endfunction
function! s:goyo_leave()
  colorscheme space-vim-dark
  Limelight!
endfunction
autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" Colour scheme selector <F8>
function! s:rotate_colors()
  if !exists('s:colors')
    let s:colors = s:colors()
  endif
  let name = remove(s:colors, 0)
  call add(s:colors, name)
  execute 'colorscheme' name
  redraw
  echo name
endfunction
nnoremap <silent> <F8> :call <SID>rotate_colors()<cr>

" :DiffSaved to show file modifications in diff format
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

" :root to change directory to git repo root
function! s:root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  if v:shell_error
    echo 'Not in git repo'
  else
    execute 'lcd' root
    echo 'Changed directory to: '.root
  endif
endfunction
command! Root call s:root()

function! CheckGitRev()
  " $(git rev-parse HEAD) == $(git rev-parse @{u})
  " rev-pase --short also works
  " git ls-remote origin -h refs/heads/master

  if expand('%:t') == '.vimrc'              " if filename = .vimrc
    call system('test -L ' . expand('%:p')) " if file = symlink
    if v:shell_error != 0                   " if file != symlink
      return
    endif
  else                                      " if filename != .vimrc
    return
  endif
  let repo = system("dirname " . system("readlink -f " . expand("%:p")))
  if (system('git -C ' . repo . ' rev-parse HEAD') != system('git -C ' . repo . ' rev-parse @{u}'))
    if confirm("Dotfiles are not up to date, would you like to fetch the latest update?", "&yes\n&No", 2) == 1
      system('git -C ' . repo . ' pull')
    endif
  endif
endfunction
autocmd VimEnter * call CheckGitRev()

" Fancy folding
" «»¶§ƒ×λ⌈⌋⟦⟧⦃⦄⨾ https://www.compart.com/en/unicode/mirrored
function! FoldText()
  set fillchars=fold:\ "
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '⦃ ' . printf("%10s", lines_count . ' lines') . ' ⦄'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('⨾' . repeat(foldchar, v:foldlevel-1) . line, 0, (winwidth(0)*2)/3)
  "let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set foldtext=FoldText()

" foldmethod=marker, syntax, indent
" vim: set foldmethod=marker foldlevel=0 nomodeline:
" }}}======================
