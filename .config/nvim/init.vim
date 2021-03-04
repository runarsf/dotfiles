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

silent! if plug#begin('~/.vim/plugged')
" General {{{
" netrw {{{
let loaded_netrwPlugin = 0  " disable netrw
let g:netrw_banner = 0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+,\(^\|\s\s\)ntuser\.\S\+'
autocmd FileType netrw set nolist
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
set conceallevel=2
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
Plug 'preservim/nerdtree' " {{{
" Open a NERDTree automatically when vim starts up if no files were specified
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | silent NERDTree | endif
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | silent NERDTree | wincmd p | endif
" Open NERDTree automatically when vim starts up on opening a directory
autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | silent NERDTree | wincmd p | ene | endif
" Close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"let g:NERDTreeDirArrowExpandable = '>'
"let g:NERDTreeDirArrowCollapsible = 'v'

" Toggle NERDTree and focus editor
map <silent> <C-n> :NERDTreeToggle <bar> wincmd p<CR>

let NERDTreeAutoDeleteBuffer = 0
let NERDTreeQuitOnOpen = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 0
let g:NERDTreeGitStatusWithFlags = 0
let NERDTreeShowHidden = 1
let g:NERDTreeWinPos = "left"
let g:NERDTreeIgnore = [
  \ '^node_modules$',
  \ '^.*\.png$',
  \ '^.*\.jpg$',
  \ '^.*\.mkv$',
  \ '^.*\.mp4$',
  \ '^.*\.mp3$'
  \ ]
"let g:WebDevIconsUnicodeDecorateFolderNodes = 1
"let g:NERDTreeGitStatusNodeColorization = 1
"let g:NERDTreeColorMapCustom = {
   "\ "Staged"    : "#0ee375",  
   "\ "Modified"  : "#d9bf91",  
   "\ "Renamed"   : "#51C9FC",  
   "\ "Untracked" : "#FCE77C",  
   "\ "Unmerged"  : "#FC51E6",  
   "\ "Dirty"     : "#FFBD61",  
   "\ "Clean"     : "#87939A",   
   "\ "Ignored"   : "#808080"   
   "\ }

"autocmd VimEnter * silent NERDTree | wincmd p
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
let g:colorscheme = 'one'
Plug 'rakr/vim-one' " {{{
let g:one_allow_italics = 1
" }}}
" }}}
call plug#end()
endif

" }}}======================
" Colours {{{
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

" Time for key combinations to time out, like <leader>w in Normal mode.
set ttimeout
set ttimeoutlen=100
set timeoutlen=400

augroup timeout | autocmd!
  autocmd InsertEnter * set timeoutlen=750
  autocmd InsertLeave * set timeoutlen=400
augroup END

set synmaxcol=250
set scrolljump=0
set nocursorline nocursorcolumn
set number relativenumber
set showcmd
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
set laststatus=2 cmdheight=1
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

" esc in insert mode, consider using kj instead, as it's no-op (up-down)
inoremap jk <esc>
" esc in command mode
cnoremap jk <C-C>

" Toggle paste
"nnoremap <leader>p :set invpaste<CR>
" Breaks if '<leader>pp' is in the pasted string
set pastetoggle=<leader>pp

nmap <silent> <leader>wr :set wrap!<CR>

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
map <leader>sp :setlocal spell! spelllang=en_us,nb<CR>:setlocal spell?<CR>

augroup twig_ft | au!
  autocmd BufRead,BufNewFile *.conf,*.toml,config setf dosini
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.txt set syntax=markdown | imap ... … | set wrap
  autocmd FileType vimwiki set wrap
  autocmd BufRead,BufNewFile *.rss set filetype=xml
  autocmd FileType python set foldmethod=indent foldnestmax=1 nomodeline
  autocmd FileType json syntax match Comment +\/\/.\+$+
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
function! ModePaste()
  return &paste ? '[p] ' : ''
endfunction
function! ModeMouse()
  return &mouse == 'a' ? '[m] ' : ''
endfunction
function! ReadOnly()
  return &readonly ? '[ro] ' : ''
endfunction
function! ModeSpell()
  return &spell ? '[sp] ' : ''
endfunction
function! ModeWrap()
  return &wrap == 'wrap' ? '[wr] ' : ''
endfunction

set statusline=

set statusline+=%F                            " current file path
set statusline+=\                             " blank space
set statusline+=%y                            " filetype
set statusline+=\                             " blank space
set statusline+=%m                            " modified flag [+]
set statusline+=\                             " blank space

set statusline+=%=                            " right-align from now on

set statusline+=%{ReadOnly()}                 " mouse flag
set statusline+=%{ModeMouse()}                " mouse flag
set statusline+=%{ModePaste()}                " paste flag
set statusline+=%{ModeWrap()}
set statusline+=%{ModeSpell()}
set statusline+=\[%{mode()}\]                 " current mode
set statusline+=\                             " blank space
set statusline+=%v                            " column number
set statusline+=\:                            " colon separator
set statusline+=%l                            " row number
set statusline+=\/                            " slash separator
set statusline+=%L                            " number of rows
set statusline+=\                             " blank space
set statusline+=%{winnr()}                    " buffer number
" }}}

" Autoreload .vimrc {{{
augroup myvimrchooks
  au!
  autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
augroup END
" }}}

function! ToggleNumbers() " {{{
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

function! s:OpenAnimatedHtop() abort " {{{
  " Open lf in a terminal
  new term://htop
  " Send window to bottom and start with small height
  wincmd J | resize 1
  " Animate height to 66%
  call animate#window_percent_height(0.66)
endfunction
command! Htop call s:OpenAnimatedHtop()
" }}}
" }}}======================
