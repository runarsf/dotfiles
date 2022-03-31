" vim: set foldmethod=marker foldlevel=0 nomodeline:
let mapleader = ','
let maplocalleader = ','

function! s:InstallVimPlug() " {{{
  let l:data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
  if empty(glob(data_dir . '/autoload/plug.vim'))
    silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    echo 'Installed vim-plug'
  else
    echo 'Vim-plug already installed'
  endif
endfunction
command! InstallVimPlug call s:InstallVimPlug()
" }}}

set nocompatible

silent! if plug#begin(stdpath('data') . '/plugged') " {{{

Plug 'tpope/vim-vinegar' " {{{
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

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

Plug 'voldikss/vim-floaterm' " {{{
nnoremap <silent> <M-S-n> :FloatermNew --height=0.9 --width=0.9 --wintype=float --name=floaterm1 --position=topright --autoclose=2<CR>
nnoremap <silent> <M-n>   :FloatermToggle<CR>
tnoremap <silent> <M-n>   <C-\><C-n>:FloatermToggle<CR>
" }}}

Plug 'nathanaelkane/vim-indent-guides' " {{{
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 1
let g:indent_guides_auto_colors = 0
"let g:indent_guides_guide_size = 2
let g:indent_guides_color_change_percent = 10
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=gray ctermbg=236
" vim-one
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#363840 ctermbg=237
" gruvbox
" autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd guibg=#282828 ctermbg=237
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
\ 'coc-eslint',
\ 'coc-pyright',
\ ]
" \ 'coc-tsserver',
" \ 'coc-vetur',
" \ 'coc-snippets',
" \ 'coc-markdownlint'
" \ 'coc-python',
" \ 'coc-emoji'
command! InstallCocExtensions exec "CocInstall -sync " . join(get(g:, 'coc_global_extensions', []))
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

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

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

if (has('nvim-0.5')) " {{{
  " Plug 'nvim-telescope/telescope.nvim'
  " Plug 'nvim-lua/plenary.nvim'
  Plug 'folke/todo-comments.nvim'
  " Plug 'kyazdani42/nvim-web-devicons'
  " Plug 'folke/trouble.nvim'
endif " }}}

Plug 'eraserhd/parinfer-rust', {'do':
        \  'cargo build --release', 'for': 'yuck'}

Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' } " {{{
let g:Hexokinase_highlighters = [
\   'sign_column',
\   'backgroundfull',
\ ]
let g:Hexokinase_optInPatterns = [
\     'full_hex',
\     'triple_hex',
\     'rgb',
\     'rgba',
\     'hsl',
\     'hsla',
\     'colour_names'
\ ]
" }}}

" Syntax highlighting {{{
Plug 'elkowar/yuck.vim', { 'for': 'yuck' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
Plug 'tpope/vim-liquid', { 'for': ['markdown', 'html'] }
Plug 'LnL7/vim-nix', { 'for': 'nix' }
Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'storyn26383/vim-vue', { 'for': 'vue' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'kovetskiy/sxhkd-vim', { 'for': 'sxhkdrc' }
Plug 'linkinpark342/xonsh-vim'
Plug 'neoclide/jsonc.vim', { 'for': ['json', 'jsonc'] }
" }}}

" Themes {{{
let s:colorscheme = 'ayu-dark'

Plug 'Shatur/neovim-ayu'

" Plug 'projekt0n/github-nvim-theme' " {{{
" lua << EOF
" require('github-theme').setup({
"   theme_style = "dark",
"   function_style = "italic",
"   sidebars = {"qf", "vista_kind", "terminal", "packer"},
" 
"   colors = {hint = "orange", error = "#ff0000"},
" 
"   overrides = function(c)
"     return {
"       htmlTag = {fg = c.red, bg = "#282c34", sp = c.hint, style = "underline"},
"       DiagnosticHint = {link = "LspDiagnosticsDefalutHint"},
"       TSField = {},
"     }
"   end
" })
" EOF
" }}}

" Plug 'joshdick/onedark.vim' " {{{
" let g:onedark_color_overrides = {
" \ "background": {"gui": "#0D1117", "cterm": "234", "cterm16": "0" },
" \}
" }}}

" }}}

call plug#end() | endif " }}}

if (has('nvim-0.5')) " {{{
" :lua print(vim.inspect(require("todo-comments.config")))
lua << EOF
require("todo-comments").setup {
  highlight = {
      pattern = [[.*<(KEYWORDS)\s*]]
    },
    search = {
      args = {
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--glob=!node_modules",
        "--glob=!lib"
      },
      pattern = [[\b(KEYWORDS)\b]]
    }
  }
  -- require("trouble").setup {}
EOF
endif " }}}

if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
if (has("termguicolors"))
  set termguicolors
endif

" Enable 256-colors, has to be set before colorscheme
set t_Co=256
set t_AB=^[[48;5;%dm
set t_AF=^[[38;5;%dm

" Specific colorscheme settings (must come after setting your colorscheme).
function! CustomHighlights() abort " {{{
  if (g:colors_name == 'gruvbox')
    if (&background == 'dark')
      highlight Visual      cterm=NONE ctermfg=NONE ctermbg=237 guibg=#3a3a3a
    else
      highlight Visual      cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
      highlight CursorLine  cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
      highlight ColorColumn cterm=NONE ctermfg=NONE ctermbg=228 guibg=#f2e5bc
    endif
  endif
  highlight Folded          guibg=#1E2030
  highlight IndentGuidesOdd ctermbg=NONE guibg=NONE
  highlight Normal          ctermbg=NONE guibg=NONE
  highlight LineNr          ctermbg=NONE guibg=NONE
  highlight SignColumn      ctermbg=NONE guibg=NONE
  highlight BufferLineFill  guibg=NONE
  "highlight Comment        cterm=italic
  "highlight TabLineFill ctermfg=LightGreen ctermbg=DarkGreen
  "highlight TabLine ctermfg=Blue ctermbg=Yellow
  "highlight TabLineSel ctermfg=Red ctermbg=Yellow
endfunction

augroup MyColors | autocmd!
  autocmd ColorScheme * call CustomHighlights()
augroup END

set background=dark
execute "colorscheme " . s:colorscheme
" }}}

" Settings {{{
"if !exists("g:syntax_on") | syntax enable | endif
syntax on
set regexpengine=0
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
set noautochdir
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
set laststatus=2
set cmdheight=1
set mat=2
set foldcolumn=0 numberwidth=1
set updatetime=300
set shortmess+=cI
set nospell
set signcolumn=yes
set list listchars=trail:·,nbsp:⎵,tab:┊» " ¦┆┊ eol:⏎ (          )
set foldmethod=marker foldmarker={{{,}}}

" Tabs and lines {{{
set autoindent
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
" }}}

set nowrap
set textwidth=0
set wrapmargin=0

" Time for key combinations to time out, like <leader>w in Normal mode.
set ttimeout
set ttimeoutlen=50 " affect Escape from insert mode delay
set timeoutlen=600

"augroup timeout | autocmd!
"  autocmd InsertEnter * set timeoutlen=300
"  autocmd InsertLeave * set timeoutlen=600
"augroup END

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

" Moving lines around in all modes
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
inoremap <C-j> <esc>:m .+1<CR>==
inoremap <C-k> <esc>:m .-2<CR>==
nnoremap <leader>j :m .+1<CR>==
nnoremap <leader>k :m .-2<CR>==

imap #dn >/dev/null<space>2>&1
inoreabbrev <expr> #!! "#!/usr/bin/env" . (empty(&filetype) ? '' : ' '.&filetype)

xnoremap <leader>b :w !bash<cr>
nnoremap <leader>B :w !bash<cr>
xnoremap <leader>p :w !python<cr>
nnoremap <leader>P :w !python<cr>

set pastetoggle=<leader>p

" don't undo entire chunk
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" when jump lines is more than 5 add a jump point
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'k'

" https://github.com/runarsf/dotfiles/blob/d695fadf5ae4704cfa0d084ec304f585ec30a679/.config/nvim/init.vim#L551
inoremap <leader><leader> <Esc>/<++><Enter>

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

if has("persistent_undo") " {{{
  " This creates unwanted shit in home dir
  " set undodir=stdpath('data').'/temp_dirs/undodir'
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
endif " }}}

function! s:statusline_expr() " {{{
  " ~/.config/nvim/init.vim [vim]   <<   >>   [unix:utf-8] 1:257/572 1
  "      "set statusline+=%#SpellHighlight#%{&spell?'[sp]':''}%*\
  "    set statusline+=
  "      "set statusline+=\[%{mode()}\]\
  let l:pth = "%f" " %F filepath
  let l:mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}" " %m
  let l:ro  = "%{&readonly ? '[RO] ' : ''}"
  let l:ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}" " %y
  let l:enc = " [%{&fileformat}:%{&fileencoding?&fileencoding:&encoding}]"
  let l:fug = "%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
  let l:sep = ' %= ' " right-align from now on
  let l:pos = ' %-12(%l : %c%V%) ' " %v\:%l\/%L\ %{winnr()}  column:row/rows window
  let l:pct = ' %P'
  let l:mus = "%{&mouse == 'a' ? '[m] ' : ''}"
  let l:pst = "%{&paste ? '[p] ' : ''}"
  let l:wrp = "%{&wrap == '1' ? '[wr] ' : ''}"
  let l:spl = "%{&spell ? '[sp] ' : ''}"

  return '[%n] '.l:pth.l:enc.' %<'.l:mod.l:ro.l:ft.l:fug.l:sep.l:pos.'%*'.l:pct.l:mus.l:pst.l:wrp
endfunction
let &statusline = s:statusline_expr()
" }}}

augroup MyVimrcHooks " {{{
  autocmd!
  " Autoreload vimrc
  autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
augroup END " }}}

function! s:ToggleFold() " {{{
  let &l:foldmethod = &foldmethod == 'marker' ? 'syntax' : 'marker'
  echo 'foldmethod is now ' . &l:foldmethod
endfunction
nmap <silent> <leader>ff :call <SID>ToggleFold()<CR>
" }}}

function! s:ToggleNumbers() " {{{
  " This function makes it easier to copy file contents using the mouse by hiding the signcolumn, line numbers, and enables wrapping while retaining the user-defined defaults.
  let b:def_signcolumn     = !exists("b:def_signcolumn")     ? "no" : b:def_signcolumn
  let b:def_number         = !exists("b:def_number")         ? 1    : b:def_number
  let b:def_relativenumber = !exists("b:def_relativenumber") ? 1    : b:def_relativenumber
  let b:def_wrap           = !exists("b:def_wrap")           ? 0    : b:def_wrap

  if &number || &relativenumber
    let b:def_number = &number
    let b:def_relativenumber = &relativenumber
    let b:def_signcolumn = &signcolumn
    set nonumber
    set list!
    set norelativenumber
    set signcolumn=no
    set wrap
  else
    if b:def_number | set number | endif
    if b:def_relativenumber | set relativenumber | endif
    if !b:def_wrap | set nowrap | endif
    set list
    execute 'set signcolumn=' . b:def_signcolumn
  endif
endfunction
nmap <silent> <leader>n :call <SID>ToggleNumbers()<CR>
" }}}

function! s:ToggleMouse() " {{{
  if &mouse == 'a'
    set mouse=c
  else
    set mouse=a
  endif
endfunction
nmap <silent> <leader>m :call <SID>ToggleMouse()<CR>
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
