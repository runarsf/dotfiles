" vim: set foldmethod=marker foldlevel=0 nomodeline: {{{
let mapleader = ','
let maplocalleader = ','
" }}}
" runarsf's .vimrc {{{
" =========================
"  vim scp://root@domain.tld//home/root/.vimrc
"  vim scp://root@domain.tld/.vimrc
"
"  vim ~/.ssh/config
"  ---
"  host shortname
"    User root
"    Hostname domain.tld
"    Port 22
"  ---
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
" vim-plug linux installation {{{
if empty(glob('~/.vim/autoload/plug.vim')) && (has('unix') || has('win32unix'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

" vim-plug windows installation {{{
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
" }}}

"filetype plugin indent on
filetype plugin indent on
let g:colorscheme = 'one'
let g:has_node = (system('node -v') =~ '^v')

silent! if plug#begin('~/.vim/plugged')
" General {{{
" Disabled General {{{
"Plug 'airblade/vim-gitgutter'
"Plug 'fcpg/vim-waikiki'
"Plug 'lervag/wiki.vim'
"Plug 'yuttie/comfortable-motion.vim'
"Plug 'psliwka/vim-smoothie'
"Plug 'tpope/vim-repeat'
"Plug 'svermeulen/vim-macrobatics'
"Plug 'michal-h21/vim-zettel'
"Plug 'michal-h21/vimwiki-sync' " Doesn't work for neovim
"if has_node && has('nvim') && !empty($DISPLAY)
"  Plug 'aurieh/discord.nvim', { 'do': ':UpdateRemotePlugins'}
"endif
"if has('python3') && has('nvim')
"  Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
"else
"  Plug 'Shougo/denite.nvim'
"  Plug 'roxma/nvim-yarp'
"  Plug 'roxma/vim-hug-neovim-rpc'
"endif
"if &rtp =~ 'denite.nvim'
"  Plug 'Shougo/vimfiler.vim'
"endif
"Plug 'SirVer/ultisnips'
"Plug 'unblevable/quick-scope'
"Plug 'AshleyF/VimSpeak'
"Plug 'fmoralesc/vim-pad'
"Plug 'metakirby5/codi.vim'
"Plug 'myusuf3/numbers.vim'
"Plug 'thaerkh/vim-workspace'
"Plug 'tpope/vim-sensible'
"Plug 'prabirshrestha/async.vim'
"Plug 'christoomey/vim-tmux-navigator'
"Plug 'davidhalter/jedi-vim'
"Plug 'gbigwood/Clippo'
"Plug 'vim-scripts/IndentAnything'
"Plug 'junegunn/vim-github-dashboard'
"Plug 'junegunn/vim-emoji'
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
"if has('nvim')
"  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"else
"  Plug 'Shougo/deoplete.nvim'
"  Plug 'roxma/nvim-yarp'
"  Plug 'roxma/vim-hug-neovim-rpc'
"endif
"let g:deoplete#enable_at_startup = 1
" }}}
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
Plug 'vimwiki/vimwiki'
Plug 'jlanzarotta/bufexplorer'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'junegunn/goyo.vim'
Plug 'justinmk/vim-sneak'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'lifepillar/vim-cheat40', { 'on': 'Cheat40' }
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
Plug 'tyru/open-browser.vim', { 'on': 'RunningX' }
"Plug 'mechatroner/rainbow_csv'
"Plug 'sheerun/vim-polyglot'
"Plug 'mbbill/undotree'
"Plug 'nathanaelkane/vim-indent-guides'
"Plug 'junegunn/vim-peekaboo'
"Plug 'tpope/vim-eunuch'
"Plug 'Xuyuanp/nerdtree-git-plugin', { 'on': 'NERDTreeToggle' }
"Plug 'zefei/vim-colortuner'
"Plug 'osyo-manga/vim-hopping'
"Plug 'danro/rename.vim', { 'on': 'Rename' }
"Plug 'junegunn/vim-easy-align'
"Plug 'terryma/vim-multiple-cursors'
"Plug 'vim-scripts/loremipsum'
"Plug 'robcsi/viewmaps.vim'
"Plug 'tpope/vim-fugitive'
"Plug 'editorconfig/editorconfig-vim'
"Plug 'tpope/vim-surround'
"Plug 'voldikss/vim-codelf', { 'on': 'Codelf' }
if g:has_node && v:version >= 703
  Plug 'neoclide/coc.nvim', { 'branch': 'release' } " 'do': { -> coc#util#install() }}
endif
Plug 'inkarkat/vim-ingo-library', { 'branch': 'stable' }
if &rtp =~ 'vim-ingo-library'
  Plug 'inkarkat/vim-ModelineCommands', { 'branch': 'stable' }
endif
Plug 'ryanoasis/vim-devicons'
" }}}
" Syntax highlighting {{{
" Disabled Syntax highlighting {{{
"Plug 'ObserverOfTime/coloresque.vim'
"if v:version >= 703
"  Plug 'scrooloose/syntastic'
"endif
"Plug 'octol/vim-cpp-enhanced-highlight', { 'for': 'cpp' }
"Plug 'nono/jquery.vim', { 'for': 'javascript' }
" }}}
Plug 'gko/vim-coloresque', { 'for': ['css', 'html', 'markdown', 'javascript', 'python'] }
Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'storyn26383/vim-vue', { 'for': 'vue' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'kovetskiy/sxhkd-vim', { 'for': 'sxhkdrc' }
Plug 'mboughaba/i3config.vim', { 'for': 'conf' }
Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'jinja' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
Plug 'dense-analysis/ale'
" }}}
" Colorschemes {{{
" Disabled Colorschemes {{{
"Plug 'ayu-theme/ayu-vim'
"Plug 'morhetz/gruvbox'
"Plug 'chriskempson/base16-vim'
"Plug 'sainnhe/edge'
"Plug 'tomasr/molokai'
"Plug 'AlessandroYorba/Despacio'
"Plug 'nightsense/cosmic_latte'
"Plug 'nightsense/snow'
"Plug 'nightsense/stellarized'
"Plug 'junegunn/seoul256.vim'
"Plug 'sjl/badwolf'
"Plug 'xero/sourcerer.vim'
"Plug 'mhartington/oceanic-next'
"Plug 'jacoborus/tender.vim'
"Plug 'joshdick/onedark.vim'
"Plug 'flrnd/plastic.vim'
"Plug 'kaicataldo/material.vim'
" }}}
Plug 'AlessandroYorba/Sierra'
Plug 'rakr/vim-one'
Plug 'liuchengxu/space-vim-dark'
Plug 'arcticicestudio/nord-vim'
" }}}
call plug#end()
endif

" netrw {{{
let loaded_netrwPlugin = 1  " disable netrw
" }}}
" base16-vim {{{
let base16colorspace=256  " Access colors present in 256 colorspace
" }}}
" edge {{{
let g:edge_style = 'neon'
let g:edge_disable_italic_comment = 0
" }}}
" material.vim {{{
let g:material_theme_style = 'default' " 'default' | 'palenight' | 'ocean' | 'lighter' | 'darker'
let g:material_terminal_italics = 1
" }}}
" vim-one {{{
let g:one_allow_italics = 1
" }}}
" tagbar {{{
nmap <F8> :TagbarToggle<CR>
" }}}
" vim-indent-guides {{{
"let g:indent_guides_enable_on_vim_startup = 1
"let g:indent_guides_auto_colors = 0
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=3
"autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=4
" }}}
" vim-codelf {{{
inoremap <silent> <leader>ce <C-R>=codelf#start()<CR>
nnoremap <silent> <leader>ce :call codelf#start()<CR>
let g:codelf_enable_popup_menu = v:true
" }}}
" deoplete.nvim {{{
"let g:deoplete#enable_at_startup = 1
" }}}
" denite.nvim {{{
"autocmd FileType denite call s:denite_my_settings()
"function! s:denite_my_settings() abort
"  nnoremap <silent><buffer><expr> <CR>
"  \ denite#do_map('do_action')
"  nnoremap <silent><buffer><expr> d
"  \ denite#do_map('do_action', 'delete')
"  nnoremap <silent><buffer><expr> p
"  \ denite#do_map('do_action', 'preview')
"  nnoremap <silent><buffer><expr> q
"  \ denite#do_map('quit')
"  nnoremap <silent><buffer><expr> i
"  \ denite#do_map('open_filter_buffer')
"  nnoremap <silent><buffer><expr> <Space>
"  \ denite#do_map('toggle_select').'j'
"endfunction
" }}}
" fzf {{{
nmap <leader>f :FZF<cr>
" }}}
" limelight.vim {{{
" Color name (:help cterm-colors) or ANSI code
"-"let g:limelight_conceal_ctermfg = 'gray'
"-"let g:limelight_conceal_ctermfg = 240
" Color name (:help gui-colors) or RGB color
"-"let g:limelight_conceal_guifg = 'DarkGray'
"-"let g:limelight_conceal_guifg = '#777777'
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
" }}}
" vim-github-dashboard {{{
"let g:github_dashboard = { 'username': 'runarsf' }
" }}}
" vim-airline {{{
"let g:airline_theme='raven'
"let g:airline#extensions#ale#enabled = 1
" }}}
" vim-sneak {{{
let g:sneak#label = 1
" }}}
" vim-emoji {{{
"command! -range EmojiReplace <line1>,<line2>s/:\([^:]\+\):/\=emoji#for(submatch(1), submatch(0))/g
" }}}
" Sierra {{{
"let g:sierra_Sunset = 1
"let g:sierra_Twilight = 1
let g:sierra_Midnight = 1
"let g:sierra_Pitch = 1
" }}}
" vim-easy-align {{{
" Start interactive EasyAlign in visual mode (e.g. vipga)
"xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
"nmap ga <Plug>(EasyAlign)
" }}}
" vim-cheat40 {{{
let g:cheat40_use_default = 1
"nmap <F1> :Cheat40<CR>
"imap <F1> :Cheat40<CR>
function! Cheat()
  "if (g:loaded_cheatsheet)
  if empty(bufname('~/.vim/plugged/vim-cheat40/cheat40.txt'))
    Cheat40
  else
    bd
  endif
endfunction
nnoremap <F1> :call Cheat()<CR>
" }}}
" gruvbox {{{
"let g:gruvbox_contrast_dark="hard"
" }}}
" nerdtree {{{
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
map <silent> <C-o> :NERDTreeToggle<CR>
" }}}
" coc.nvim {{{
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
" }}}
" }}}======================
" A E S T H E T I C S {{{
" =========================
" Tab colors
highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
highlight TabLine ctermfg=Blue ctermbg=Yellow
highlight TabLineSel ctermfg=Red ctermbg=Yellow

" For Neovim 0.1.3 and 0.1.4 - https://github.com/neovim/neovim/pull/2198
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

" For Neovim > 0.1.5 and Vim > patch 7.4.1799 - https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162
" Based on Vim patch 7.4.1770 (`guicolors` option) - https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd
" https://github.com/neovim/neovim/wiki/Following-HEAD#20160511
if (has('termguicolors'))
  set termguicolors
endif

" Enable 256-colors, has to be set before colorscheme
set t_Co=256
set t_AB=^[[48;5;%dm
set t_AF=^[[38;5;%dm

" Colorscheme
set background=dark
execute "colorscheme " . g:colorscheme

highlight Comment cterm=italic
"highlight Normal     ctermbg=NONE guibg=NONE
"highlight LineNr     ctermbg=NONE guibg=NONE
"highlight SignColumn ctermbg=NONE guibg=NONE

"if has('nvim')
  " https://github.com/neovim/neovim/issues/2897#issuecomment-115464516
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
" General {{{
" =========================
"set textwidth=80                                             " Make it obvious where 80 characters is
"set colorcolumn=+1
"set nojoinspaces                                             " Use one space, not two, after punctuation.
set history=200                                               " Sets how many lines of history VIM has to remember
set autoread                                                  " Set to auto read when a file is changed from the outside
set clipboard+=unnamedplus
"set wrap                                                     " Enables wrapping
set nowrap
set textwidth=0                                               " Disable wrapping
set wrapmargin=0
"set foldlevelstart=99                                        " Start with fold level 99 at launch (all folds closed)
"set foldmethod=syntax
"if expand('%:t') == '.vimrc' | set foldmethod=marker | else | set foldmethod=syntax | endif
"set foldlevel=0
"set modelines=0                                               " Disable modelines as a security precaution<Paste>
"set nomodeline
set modeline
set modelines=5
set nocompatible                                              " Enables VI iMproved enhancements
"set guifont=Source\ Code\ Pro                                  " GUI Font
"syntax on                                                     " Enable syntax highlighting
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
set synmaxcol=180
set scrolljump=5
set nocursorline                                              " Highlight current line
set nocursorcolumn
set number                                                    " Enable line numbers (or absolute line number on current line with relativenumber)
set relativenumber                                            " Set line numbers to relative
set ruler
set showcmd                                                   " Display incomplete commands
set scrolloff=5                                                      " Set lines to the cursor - when moving vertically
let $LANG='en'                                                " Avoid garbled characters in Chinese language in Windows
set langmenu=en
set wildmenu                                                  " Turn on the Wild menu for cycling through command options
set wildmode=longest:full,full                                " longest:list,full
set cmdheight=1                                               " Height of the command bar
set hidden                                                    " A buffer becomes hidden when it is abandoned, recommended for coc
set backspace=indent,eol,start                                " Configure backspace so it acts as it should act
set whichwrap+=<,>,h,l
set ignorecase                                                " Make search case insensitive
set smartcase                                                 " When searching try to be smart about cases
set lazyredraw                                                " Don't redraw while executing macros (good performance config)
set magic                                                     " For regular expressions turn magic on
set incsearch                                                 " Makes search act like search in modern browsers
set noshowmatch                                                 " Show matching brackets when text indicator is over them
set hlsearch                                                  " Highlight search results
set mat=2                                                     " How many tenths of a second to blink when matching brackets
set foldcolumn=0                                              " Left margin
set numberwidth=1                                             " Left margin
set updatetime=300                                            " Default 4000
set shortmess+=c                                              " don't give |ins-completion-menu| messages.
"set signcolumn=yes                                           " always show signcolumns
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
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

" Insert tab (i CTRL+v TAB)
"nmap <leader>t i	<ESC>
"imap <leader>t

" Disable CTRL-A on tmux or on screen
if $TERM =~ 'screen'
  nnoremap <C-a> <nop>
  nnoremap <leader><C-a> <C-a>
endif

" space open/closes folds
nnoremap <space> za

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
"nnoremap <F5> :echo system('python3 "' . expand('%') . '"')<cr>

" Toggle paste
"nnoremap <leader>p :set invpaste<CR>
" Breaks if '<leader>p' is in the pasted string
set pastetoggle=<leader>p

" Rebind CapsLock to Escape in X-Sessions
if has('unix') && !empty($DISPLAY)
  autocmd VimEnter * silent! !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
  autocmd VimLeave * silent! !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Caps_Lock'
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
nmap <leader>cfg :e ~/.vimrc<CR>

" Fast saving
nmap <leader>w :w!<CR>

" Write quit
nmap <leader>wq :wq<CR>

" Fast quit
nmap <leader>q :q<CR>
nmap <leader>Q :q!<CR>

" Toggle Zen mode / Goyo
nmap <leader>z :Goyo<CR>

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
"vnoremap <C-c> "*Y :let @+=@*<CR>
"map <C-p> "+P

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
"if has("gui_running")
"  set guioptions-=T
"  set guioptions-=e
"  set guitablabel=%M\ %t
"  set guioptions-=r                " Disable scrollbars
"  set guioptions-=R
"  set guioptions-=l
"  set guioptions-=L
"else
"  set t_Co=256
"endif

" File interpreting
autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile .{jscs,jshint,eslint}rc set filetype=json
autocmd BufRead,BufNewFile *.zsh-theme,aliases.local,zshrc.local,*/zsh/configs/* set filetype=zsh
autocmd BufRead,BufNewFile gitconfig.local set filetype=gitconfig
autocmd BufRead,BufNewFile tmux.conf.local set filetype=tmux
autocmd BufRead,BufNewFile vimrc.local set filetype=vim
autocmd BufRead,BufNewFile *.rss set filetype=xml

" Turn persistent undo on
" Undo even when you close a buffer/VIM
try
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
catch
endtry

" Automatically deletes all trailing whitespace on save
"autocmd BufWritePre * %s/\s\+$//e

" Save and restore code folding
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent! loadview

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" }}}======================
" Functions / Utilities {{{
" =========================
" :W sudo saves file {{{
command! W w !sudo tee % > /dev/null
" }}}

" Create command aliases {{{
fun! SetupCommandAlias(from, to)
  exec 'cnoreabbrev <expr> '.a:from
        \ .' ((getcmdtype() is# ":" && getcmdline() is# "'.a:from.'")'
        \ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfun
" call SetupCommandAlias('W','w')
" }}}

" Run line under cursor {{{
"nnoremap <leader>o mmI:<esc>v$h"oy@o<CR>x`m
nnoremap <leader>o "oyy:exe @o<CR>
" }}}

" Autoreload .vimrc {{{
augroup myvimrchooks
  au!
  autocmd bufwritepost .vimrc source ~/.vimrc
augroup END
" }}}

" Dynamic line numbers {{{
"function! ToggleNumbers()
"  if &number || &relativenumber
"    call EnterInsert()
"    set nonumber
"    set norelativenumber
"  else
"    call LeaveInsert()
"  endif
"endfunction
"nmap <silent> <leader>n :call ToggleNumbers()<CR>

"function! EnterInsert()
"  "GitGutterDisable
"  set cursorline
"  set norelativenumber
"  set number
"endfunction
"function! LeaveInsert()
"  "GitGutterEnable
"  set nocursorline
"  set relativenumber
"  set number
"endfunction
"autocmd InsertEnter * call EnterInsert()
"autocmd FocusLost * call EnterInsert()
"autocmd InsertLeave * call LeaveInsert()
"autocmd FocusGained * call LeaveInsert()
"autocmd VimEnter * call LeaveInsert()
" }}}

" Switch colorscheme and enable limelight with Goyo {{{
function! s:goyo_enter()
  colorscheme sierra
  Limelight
endfunction
function! s:goyo_leave()
  execute "colorscheme " . g:colorscheme
  "highlight Normal     ctermbg=NONE guibg=NONE
  Limelight!
endfunction
autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
" }}}

" :DiffSaved to show file modifications in diff format {{{
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()
" }}}

" :root to change directory to git repo root {{{
function! s:root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  if v:shell_error
    echo 'Not in git repo'
  else
    execute 'lcd' root
    echo 'Changed directory to: ' . root
  endif
endfunction
command! Root call s:root()
" }}}

" Fancy folding {{{
function! FoldText()
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
" }}}======================
" Terminal handling {{{
" =========================
" Set login shell for :terminal command so aliases work
set shell=/bin/zsh

" Creates a floating window with a most recent buffer to be used https://github.com/camspiers/dotfiles/blob/master/files/.config/nvim/init.vim#L446-L468 {{{
function! CreateCenteredFloatingWindow()
  let width = float2nr(&columns * 0.6)
  let height = float2nr(&lines * 0.6)
  let top = ((&lines - height) / 2) - 1
  let left = (&columns - width) / 2
  let opts = {'relative': 'editor', 'row': top, 'col': left, 'width': width, 'height': height, 'style': 'minimal'}

  let top = "┌" . repeat("─", width - 2) . "┐"
  "let top = "╭" . repeat("─", width - 2) . "╮"
  let mid = "│" . repeat(" ", width - 2) . "│"
  "let bot = "╰" . repeat("─", width - 2) . "╯"
  let bot = "└" . repeat("─", width - 2) . "┘"
  let lines = [top] + repeat([mid], height - 2) + [bot]
  let s:buf = nvim_create_buf(v:false, v:true)
  call nvim_buf_set_lines(s:buf, 0, -1, v:true, lines)
  call nvim_open_win(s:buf, v:true, opts)
  set winhl=Normal:Floating
  let opts.row += 1
  let opts.height -= 2
  let opts.col += 2
  let opts.width -= 4
  call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
  au BufWipeout <buffer> exe 'bw '.s:buf
endfunction
" }}}

" When term starts, auto go into insert mode
"autocmd TermOpen * startinsert
" Turn off line numbers etc
"autocmd TermOpen * setlocal listchars= nonumber norelativenumber

function! OpenTerm(cmd)
  call CreateCenteredFloatingWindow()
  call termopen(a:cmd, { 'on_exit': function('TermExitCallback') })
endfunction

function! TermExitCallback(job_id, code, event) dict
  if a:code == 0
    bd!
  endif
endfunction

"nnoremap <C-n> :call ToggleScratchTerm()<CR>
function! ToggleScratchTerm()
  if empty(bufname('/bin/zsh'))
    call OpenTerm('/bin/zsh')
  else
    bd!
  endif
endfunction

function! ToggleLazyGit()
  if empty(bufname('lazygit'))
    call OpenTerm('lazygit')
  else
    bd!
  endif
endfunction
" }}}======================
" Vimwiki (sync) {{{
" =========================
" vim-wiki (plugin settings) {{{
"let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
let g:vimwiki_list = [{'path': '~/wiki/', 'path_html': '~/wiki/src/templates/'}]
"let g:vimwiki_list = [{'path': '~/wiki/',
"                      \ 'syntax': 'markdown', 'ext': '.md'}]
" }}}

function! WikiSyncPull()
  let l:wd = expand(g:vimwiki_list[0]['path'])
  silent execute "!git -C " . l:wd . " pull"
endfunction

" Requires ssh-agent to be added for push to work
" `eval "$(ssh-agent)" && ssh-add`
function! WikiSyncPush()
  let l:wd = expand(g:vimwiki_list[0]['path'])
  let l:htmlwd = expand(g:vimwiki_list[0]['path_html'])
  " Compile Wiki to HTML - https://github.com/vimwiki/vimwiki/blob/master/ftplugin/vimwiki.vim#L252
  call vimwiki#html#WikiAll2HTML(expand(l:htmlwd))
  " Push changes to git (`call system('command')` doesn't show output) and check that it succeeded
  if confirm("Push changes to git?", "&Yes\n&No", 1) == 1
    silent execute "!git -C " . l:wd . " pull"
    silent execute "!git -C " . l:wd . " add ."
    silent execute "!git -C " . l:wd . " commit -m\'auto push\'"
    silent execute "!git -C " . l:wd . " push origin master"

    if !(system("git -C " . l:wd . " status") =~ "up to date")
      if confirm("Could not push to git, retry and add ssh-agent?", "&Yes\n&No", 2) == 1
        execute '!eval "$(ssh-agent)" && ssh-add'
        call WikiSyncPush()
      endif
    endif
  endif
endfunction

augroup vimwikiSync
  autocmd!
  " Pull changes when entering wiki/index.wiki
  execute "autocmd BufWinEnter " . expand(g:vimwiki_list[0]['path'] . "index.wiki") . " call WikiSyncPull()"
  " Alias :wq to :w :q when entering wiki/index.wiki (note: this will still be set if you open a new tab/buffer (?))
  execute "autocmd BufWinEnter " . expand(g:vimwiki_list[0]['path'] . "index.wiki") . " cnoreabbrev wq write <bar> quit"
  " Compile HTML and push changes when writing to wiki/index.wiki
  execute "autocmd BufWritePost,FileWritePost " . expand(g:vimwiki_list[0]['path'] . "index.wiki") . " call WikiSyncPush()"
augroup END
" }}}======================
