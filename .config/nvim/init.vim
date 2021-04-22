" vim: set foldmethod=marker foldlevel=0 nomodeline: {{{
set runtimepath+=~/.vim,~/.vim/after
set packpath+=~/.vim

let mapleader = ','
let maplocalleader = ','
" }}}
" Plugins {{{
" =========================
" vim-plug linux installation {{{
if (has('unix') || has('win32unix')) && empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}
" vim-plug windows installation {{{
if has('win32') && empty(glob('~\vimfiles\autoload\plug.vim')) && empty(glob('~\AppData\Local\nvim\autoload\plug.vim'))
  if empty(glob('$LOCALAPPDATA\nvim\autoload\plug.vim'))
  silent ! powershell -Command "
  \   New-Item -Path ~\AppData\Local\nvim -Name autoload -Type Directory -Force;
  \   Invoke-WebRequest
  \   -Uri 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  \   -OutFile ~\AppData\Local\nvim\autoload\plug.vim
  \ "
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
"  if has('nvim')
"    md ~\AppData\Local\nvim\autoload
"    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
"    (New-Object Net.WebClient).DownloadFile(
"      $uri,
"      $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(
"        "~\AppData\Local\nvim\autoload\plug.vim"
"      )
"    )
"  else
"    md ~\vimfiles\autoload
"    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
"    (New-Object Net.WebClient).DownloadFile(
"      $uri,
"      $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(
"        "~\vimfiles\autoload\plug.vim"
"      )
"    )
"  endif
"  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

set nocompatible
if has("autocmd")
  filetype plugin indent on
endif

" call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
silent! if plug#begin('~/.vim/plugged')
" General {{{
" netrw {{{
Plug 'tpope/vim-vinegar'
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

" https://stackoverflow.com/a/23920240
map <silent> <C-n> :Lexplore<CR>

"map <silent> <C-n> :NetrwToggle <bar> wincmd p<CR>

" https://vi.stackexchange.com/questions/10988/toggle-explorer-window
" }}}
Plug 'mhinz/vim-startify' " {{{
let g:startify_session_dir = '~/.config/nvim/session'
let g:startify_lists = [
          \ { 'type': 'files',     'header': [ '   Files' ] },
          \ { 'type': 'dir',       'header': [ '   Current Directory ' . getcwd() ] },
          \ { 'type': 'bookmarks', 'header': [ '   Bookmarks' ] },
          \ { 'type': 'sessions',  'header': [ '   Sessions' ] }
          \ ]
let g:startify_bookmarks = [
            \ { 'h': expand('~') },
            \ { 'n': '~/Documents/drive/obsidian' },
            \ { 'c': '~/.config/nvim/init.vim' },
            \ { 'z': '~/.config/zsh/.zshrc' }
            \ ]
let g:startify_change_to_dir = 0
let g:startify_change_to_vcs_root = 1
let g:startify_session_autoload = 1
let g:startify_session_delete_buffers = 1
let g:startify_fortune_use_unicode = 1
let g:startify_session_persistence = 1
let g:startify_enable_special = 0
autocmd User Startified setlocal cursorline
"let g:startify_custom_header = 'startify#center(startify#fortune#boxed())'
let g:startify_custom_header = []

" }}}
Plug 'voldikss/vim-floaterm' " {{{
nmap <F5> :FloatermNew --height=0.6 --width=0.4 --wintype=float --name=Terminal --position=bottomright<CR>
nmap <F6> :FloatermKill!<CR>
" }}}
Plug 'iamcco/markdown-preview.vim'
"Plug 'dense-analysis/ale' " {{{
"let g:ale_fix_on_save = 0
"let g:ale_fixers = {
"\   '*': ['remove_trailing_lines', 'trim_whitespace'],
"\   'javascript': ['eslint'],
"\}
"" }}}
"Plug 'vim-syntastic/syntastic' " {{{
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
"" }}}
Plug 'lervag/vimtex' " {{{
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:tex_conceal='abdmg'
" }}}
Plug 'sirver/ultisnips' " {{{
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsEditSplit="vertical"
" }}}
Plug 'reedes/vim-textobj-quote' | Plug 'kana/vim-textobj-user' " {{{
augroup textobj_quote | autocmd!
  autocmd FileType markdown call textobj#quote#init()
  autocmd FileType textile call textobj#quote#init()
  autocmd FileType text call textobj#quote#init({'educate': 0})
augroup END
" }}}
Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown' " {{{
" https://github.com/plasticboy/vim-markdown
let g:vim_markdown_folding_disabled = 0
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_folding_level = 6
let g:vim_markdown_no_default_key_mappings = 1
let g:vim_markdown_toc_autofit = 1
let g:vim_markdown_emphasis_multiline = 1
let g:vim_markdown_conceal = 1
let g:vim_markdown_conceal_code_blocks = 1
let g:vim_markdown_fenced_languages = ['c++=cpp', 'viml=vim', 'bash=sh', 'ini=dosini']
"let g:vim_markdown_follow_anchor = 0
"let g:vim_markdown_anchorexpr = "'<<'.v:anchor.'>>'"
let g:vim_markdown_strikethrough = 1
let g:vim_markdown_new_list_item_indent = 2
"let g:vim_markdown_no_extensions_in_markdown = 1
let g:vim_markdown_autowrite = 1
let g:vim_markdown_auto_extension_ext = 'md'
let g:vim_markdown_auto_insert_bullets = 1
let g:vim_markdown_new_list_item_indent = 1
let g:vim_markdown_edit_url_in = 'tab' " tab, vsplit, hsplit, current
" }}}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } | Plug 'junegunn/fzf.vim' | Plug 'camspiers/animate.vim' " {{{
nmap <silent> <leader>f :FZF<CR>
let g:fzf_layout = {
 \ 'window': 'new | wincmd J | resize 1 | call animate#window_percent_height(0.5)'
\ }
" }}}
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' } " {{{
let g:Hexokinase_highlighters = ['virtual']
let g:Hexokinase_v2 = 0
autocmd! VimEnter * HexokinaseRefresh
" blue
" }}}
"Plug 'preservim/nerdtree' " {{{
"" Open a NERDTree automatically when vim starts up if no files were specified
""autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | silent NERDTree | endif
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | silent NERDTree | wincmd p | endif
"" Open NERDTree automatically when vim starts up on opening a directory
"autocmd StdinReadPre * let s:std_in=1
""autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
""autocmd StdinReadPre * let s:std_in=1
""autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | silent NERDTree | wincmd p | ene | endif
"" Close vim if the only window left open is a NERDTree
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
"
""let g:NERDTreeDirArrowExpandable = '>'
""let g:NERDTreeDirArrowCollapsible = 'v'
"
"" Toggle NERDTree and focus editor
"map <silent> <C-n> :NERDTreeToggle <bar> wincmd p<CR>
"
"let NERDTreeAutoDeleteBuffer = 0
"let NERDTreeQuitOnOpen = 1
"let NERDTreeMinimalUI = 1
"let NERDTreeDirArrows = 0
"let g:NERDTreeGitStatusWithFlags = 0
"let NERDTreeShowHidden = 1
"let g:NERDTreeWinPos = "left"
"let g:NERDTreeIgnore = [
"  \ '^node_modules$',
"  \ '^.*\.png$',
"  \ '^.*\.jpg$',
"  \ '^.*\.mkv$',
"  \ '^.*\.mp4$',
"  \ '^.*\.mp3$'
"  \ ]
""let g:WebDevIconsUnicodeDecorateFolderNodes = 1
""let g:NERDTreeGitStatusNodeColorization = 1
""let g:NERDTreeColorMapCustom = {
"   "\ "Staged"    : "#0ee375",  
"   "\ "Modified"  : "#d9bf91",  
"   "\ "Renamed"   : "#51C9FC",  
"   "\ "Untracked" : "#FCE77C",  
"   "\ "Unmerged"  : "#FC51E6",  
"   "\ "Dirty"     : "#FFBD61",  
"   "\ "Clean"     : "#87939A",   
"   "\ "Ignored"   : "#808080"   
"   "\ }
"
""autocmd VimEnter * silent NERDTree | wincmd p
" }}}
Plug 'nathanaelkane/vim-indent-guides' " {{{
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 1
let g:indent_guides_auto_colors = 0
"let g:indent_guides_guide_size = 2
let g:indent_guides_color_change_percent = 10
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=gray ctermbg=236
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#363840 ctermbg=237
" }}}
if v:version >= 703 && executable('node') | Plug 'neoclide/coc.nvim', {'branch': 'release'} " {{{
"if v:version >= 703 && executable('node') && executable('yarn') | Plug 'neoclide/coc.nvim', { 'branch': 'release', 'tag': '*', 'do': { -> coc#util#install()}}
let g:coc_global_extensions = [
  \ 'coc-pairs',
  \ 'coc-html',
  \ 'coc-json',
  \ 'coc-css',
  \ 'coc-yaml',
  \ 'coc-highlight',
  \ 'coc-tabnine',
  \ ]
  " \ 'coc-vetur',
  " \ 'coc-snippets',
  " \ 'coc-markdownlint'
  " \ 'coc-python',
  " \ 'coc-emoji'
function! NiceCock()
  " https://stackoverflow.com/a/13908273
  let coc_extensions=glob(fnameescape(expand('~/.config/coc/extensions/node_modules')).'/{,.}*/', 1, 1)
  call map(coc_extensions, 'fnamemodify(v:val, ":h:t")')

  for extension in coc_extensions
    if index(g:coc_global_extensions, extension) == -1
      exec "CocUninstall " . extension
    endif
  endfor
endfunction
command! Cock exec "CocInstall -sync " . join(get(g:, 'coc_global_extensions', []))
command! Caulk call NiceCock()
" Use `[g` and `]g` to navigate diagnostics
nmap <silent> <space>G <Plug>(coc-diagnostic-prev)
nmap <silent> <space>g <Plug>(coc-diagnostic-next)
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
endif
" }}}
" }}}
" Syntax highlighting {{{
Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'storyn26383/vim-vue', { 'for': 'vue' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'kovetskiy/sxhkd-vim', { 'for': 'sxhkdrc' }
" }}}
" Colorschemes {{{
"let g:colorscheme = 'iceburg'
let g:colorscheme = 'one'
"Plug 'arcticicestudio/nord-vim'
Plug 'rakr/vim-one' " {{{
let g:one_allow_italics = 1
" }}}
" }}}
call plug#end()
endif

" }}}======================
" Aesthetics {{{
" =========================
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

function! MyHighlights() abort
  highlight Comment cterm=italic
  highlight Normal     ctermbg=NONE guibg=NONE
  highlight LineNr     ctermbg=NONE guibg=NONE
  highlight SignColumn ctermbg=NONE guibg=NONE
  highlight FloatermBorder guibg=#13141d
  "highlight BufferLineFill guibg=NONE
  "highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
  "highlight TabLine ctermfg=Blue ctermbg=Yellow
  "highlight TabLineSel ctermfg=Red ctermbg=Yellow
endfunction

augroup MyColors | autocmd!
  autocmd ColorScheme * call MyHighlights()
augroup END

" Colorscheme
set background=dark
execute "colorscheme " . g:colorscheme

" }}}======================
" General {{{
" =========================

" Performance, force vim to fall back to the old regex engine
set re=1

"set autochdir " Change directory to the current buffer when opening files.
set clipboard+=unnamedplus
set history=500
set autoread
set modeline modelines=5
if !exists("g:syntax_on") | syntax enable | endif
set encoding=utf-8
set ffs=unix,dos,mac
set nobackup nowritebackup noswapfile
set splitbelow splitright
"set complete-=i
set mouse=c
set noerrorbells novisualbell
set t_vb=
set conceallevel=2

" Time for key combinations to time out, like <leader>w in Normal mode.
set ttimeout
set ttimeoutlen=50 " affect Escape from insert mode delay
set timeoutlen=600

augroup timeout | autocmd!
  autocmd InsertEnter * set timeoutlen=300
  autocmd InsertLeave * set timeoutlen=600
augroup END

set synmaxcol=250
set scrolljump=0
set nocursorline nocursorcolumn
set number relativenumber
set showcmd
set showfulltag
set showmatch
set scrolloff=7
set langmenu=en
set wildmenu
set wildmode=longest:full,full
set path+=**
set wildignore+=**/node_modules/**

" Allow switching buffers without writing
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
set signcolumn=no
set list listchars=trail:·,nbsp:⎵,tab:┊» " ¦┆┊ eol:⏎ (		)
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

"au BufNewFile,BufRead *.py
"au Filetype python
"  \ setlocal tabstop=4
"  \ | setlocal softtabstop=4
"  \ | setlocal shiftwidth=4
"  \ | setlocal textwidth=79
"  \ | setlocal fileformat=unix
"\ set autoindent
" }}}

" oni {{{
if exists('g:gui_oni')
  set noswapfile
  set smartcase
  set mouse=a
  set noshowmode
  set noruler
  set laststatus=0
  set noshowcmd
endif
" }}}

source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
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

" LaTeX {{{
" https://github.com/LukeSmithxyz/voidrice/commit/a89697cd752329aa5fe36c82655862cb09a984ac#diff-53d6aeb99314b91adfd5bfd4b227589b
" https://github.com/LukeSmithxyz/voidrice/blob/aa7a4af4612462f30668a7d161b2b75c9582bfcf/.config/nvim/init.vim
" inoremap <expr> <leader><leader> exists('b:texmap') ? '<Esc>/<++><Enter>"_c4l<Esc>\:unlet b\:texmap' : '<leader><leader>'
inoremap <leader><leader> <Esc>/<++><Enter>"_c4l

" Always open in tex filetype instead of plaintex
"let g:tex_flavor = "latex"
"
"augroup LaTeX | autocmd!
"  autocmd FileType tex,latex,plaintex,bib inoremap <leader><leader> <Esc>/<++><Enter>"_c4l
"  autocmd FileType tex,latex,plaintex,bib vnoremap <leader><leader> <Esc>/<++><Enter>"_c4l
"  autocmd FileType tex,latex,plaintex,bib map <leader><leader> <Esc>/<++><Enter>"_c4l
"
"  autocmd FileType tex inoremap <buffer> ,em \emph{}<++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,dc \documentclass{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,be \begin{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,en \end{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,bf \textbf{}<++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,it \textit{}<++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,ct \textcite{}<++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,cp \parencite{}<++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,glos {\gll<Space><++><Space>\\<Enter><++><Space>\\<Enter>\trans{``<++>''}}<Esc>2k2bcw
"  autocmd FileType tex inoremap <buffer> ,x \begin{xlist}<Enter>\ex<Space><Enter>\end{xlist}<Esc>kA<Space>
"  autocmd FileType tex inoremap <buffer> ,ol \begin{enumerate}<Enter><Enter>\end{enumerate}<Enter><Enter><++><Esc>3kA\item<Space>
"  autocmd FileType tex inoremap <buffer> ,ul \begin{itemize}<Enter><Enter>\end{itemize}<Enter><Enter><++><Esc>3kA\item<Space>
"  autocmd FileType tex inoremap <buffer> ,li <Enter>\item<Space>
"  autocmd FileType tex inoremap <buffer> ,ref \ref{}<Space><++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,a \href{}{<++>}<Space><++><Esc>2T{i
"  autocmd FileType tex inoremap <buffer> ,sc \textsc{}<Space><++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,chap \chapter{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,sec \section{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,ssec \subsection{}<Enter><Enter><++><Esc>2kf}i
"  autocmd FileType tex inoremap <buffer> ,sssec \subsubsection{}<Enter><Enter><++><Esc>2kf}i
"  "autocmd FileType tex inoremap <buffer> ,up <Esc>/usepackage<Enter>o\usepackage{}<Esc>i
"  "autocmd FileType tex nnoremap <buffer> ,up /usepackage<Enter>o\usepackage{}<Esc>i
"  autocmd FileType tex inoremap <buffer> ,tt \texttt{}<Space><++><Esc>T{i
"  autocmd FileType tex inoremap <buffer> ,bt {\blindtext}
"  autocmd FileType bib inoremap <buffer> ,a @article{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>journal<Space>=<Space>{<++>},<Enter>volume<Space>=<Space>{<++>},<Enter>pages<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i
"  autocmd FileType bib inoremap <buffer> ,b @book{<Enter>author<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>6kA,<Esc>i
"  autocmd FileType bib inoremap <buffer> ,c @incollection{<Enter>author<Space>=<Space>{<++>},<Enter>title<Space>=<Space>{<++>},<Enter>booktitle<Space>=<Space>{<++>},<Enter>editor<Space>=<Space>{<++>},<Enter>year<Space>=<Space>{<++>},<Enter>publisher<Space>=<Space>{<++>},<Enter>}<Enter><++><Esc>8kA,<Esc>i
"augroup END
" }}}

let g:markdown_folding = 1

" Use ctrl-[hjkl] to select the active split!
" https://vim.fandom.com/wiki/Switch_between_Vim_window_splits_easily
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Tab navigation
nnoremap H gT
nnoremap L gt
nnoremap <C-t> :tabnew<CR>

" Disable CTRL-A on tmux and screen
if $TERM =~ 'screen'
  nnoremap <C-a> <nop>
  nnoremap <leader><C-a> <C-a>
endif

nmap <leader>.. :messages<CR>

nnoremap <silent> <leader>l :set cursorline!<CR>
nnoremap <silent> <leader>ll :set cursorcolumn!<CR>

" Toggle paste
"nnoremap <leader>p :set invpaste<CR>
" Breaks if '<leader>pp' is in the pasted string
set pastetoggle=<leader>p

"nmap <silent> <leader>r :set relativenumber!<CR>
"nmap <silent> <leader>rn :set number!<CR>

nnoremap <silent> <leader><space> :nohlsearch<CR>

" Fast config edit
nmap <leader>cfg :e $MYVIMRC<CR>

" Fast file actions
nmap <leader>w :w!<CR>
nmap <leader>wq :wq<CR>
nmap <leader>q :q<CR>
nmap <leader>Q :q!<CR>

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <silent> <M-j> mz:m+<cr>`z
nmap <silent> <M-k> mz:m-2<cr>`z
vmap <silent> <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <silent> <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

imap #dn >/dev/null<space>2>&1
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)

" Spell-check, english and norwegian
set nospell
map <leader>sp :setlocal spell! spelllang=en_us,nb<CR>:setlocal spell?<CR>
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
map <leader>f 1z=
imap <leader>f <Esc>1z=i

function! EnableFasterInsertTimeout()
  " Enables faster timeoutlen in insert mode in the current buffer
  " Pairs with the default timeout augroup
  " https://vi.stackexchange.com/a/4123
  augroup timeout | autocmd! * <buffer>
    autocmd BufEnter    <buffer> set timeoutlen=500
    autocmd InsertEnter <buffer> set timeoutlen=250
    autocmd InsertLeave <buffer> set timeoutlen=500
  augroup END
endfunction

" markdown preview alternative, zathura
nnoremap <leader>mc :execute '!${XDG_CONFIG_HOME:-${HOME}/.config}/nvim/mdconvert.sh %:p' <bar> :redraw! <Enter>

" Navigate inside wrapped lines
" https://vim.fandom.com/wiki/Move_cursor_by_display_lines_when_wrapping
"nmap <silent> <leader>wr :set wrap!<CR>
"noremap <silent> <Leader>wr :call ToggleWrap()<CR>
"function ToggleWrap(...)
"  let s:state = get(a:, 1, 'off')
"  if &wrap && s:state != 'on'
"    echo "Wrap OFF"
"    setlocal nowrap
"    set virtualedit=all
"    silent! nunmap <buffer> <Up>
"    silent! nunmap <buffer> <Down>
"    silent! nunmap <buffer> <Home>
"    silent! nunmap <buffer> <End>
"    silent! iunmap <buffer> <Up>
"    silent! iunmap <buffer> <Down>
"    silent! iunmap <buffer> <Home>
"    silent! iunmap <buffer> <End>
"  else
"    "state != 'off'
"    echo "Wrap ON"
"    setlocal wrap linebreak nolist
"    set virtualedit=
"    setlocal display+=lastline
"    noremap  <buffer> <silent> <Up>   gk
"    noremap  <buffer> <silent> <Down> gj
"    noremap  <buffer> <silent> <Home> g<Home>
"    noremap  <buffer> <silent> <End>  g<End>
"    inoremap <buffer> <silent> <Up>   <C-o>gk
"    inoremap <buffer> <silent> <Down> <C-o>gj
"    inoremap <buffer> <silent> <Home> <C-o>g<Home>
"    inoremap <buffer> <silent> <End>  <C-o>g<End>
"  endif
"endfunction


augroup filetype_tweaks | autocmd!
  " Assign filetypes
  autocmd BufRead,BufNewFile *.conf,*.toml,config set filetype=dosini
  autocmd BufRead,BufNewFile *.md,*.rmd,*.txt set filetype=markdown
  autocmd BufRead,BufNewFile *.rss set filetype=xml
  autocmd BufRead,BufNewFile commands set filetype=sh

  " Modify filetypes
  autocmd FileType help set
    \ nospell
  autocmd FileType markdown,tex set
    \ wrap
    \ linebreak
    \ autoindent
    \ colorcolumn=0
    \ nonumber
    \ shiftwidth=2
    \ spell
    \ tabstop=2
    \ synmaxcol=800
    \ | imap ... …
    \ | imap -- —
    \ | imap --- –
    \ | call EnableFasterInsertTimeout()
  autocmd FileType tex
    \ inoremap <C-i> \textit{}<Esc>i
    \ | inoremap <C-b> \textbf{}<Esc>i
    \ | inoremap <C-u> \underline{}<Esc>i
    \ | inoremap <C-e> \emph{}<Esc>i
  autocmd FileType python set
    \ foldmethod=indent
    \ foldnestmax=1
    \ nomodeline
  autocmd FileType json set
    \ conceallevel=0
    \ | syntax match Comment +\/\/.\+$+
augroup END
" }}}======================
" Functions / Utilities {{{
" =========================
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
"try
"  set undodir=~/.vim_runtime/temp_dirs/undodir
"  set undofile
"catch
"endtry
if has("persistent_undo")
  set undodir=~/.vim_runtime/temp_dirs/undodir
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
augroup myvimrchooks
  au!
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

function! s:DiffWithSaved() " {{{
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
command! DiffSaved call s:DiffWithSaved()
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

function! s:Draw() abort " {{{
  " https://vim.fandom.com/wiki/Display_output_of_shell_commands_in_new_window
  " https://vim.fandom.com/wiki/Append_output_of_an_external_command
  " https://github.com/runarsf/dotfiles/blob/3ab666e7ee92f1ddf3c00091304f43e604db2c00/.config/nvim/init.vim

  "new term://tdrawvim
  "execute "!tdraw"
  split | wincmd J | resize 1
  let @d = system("tdraw")
  setlocal norelativenumber
  setlocal nonumber
  setlocal mouse=a
  startinsert
  call animate#window_percent_height(0.66)
endfunction
command! Draw call s:Draw()
" }}}
" }}}======================
