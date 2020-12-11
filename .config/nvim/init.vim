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
let g:colorscheme = 'one'

silent! if plug#begin('~/.vim/plugged')
" Built-in {{{
" netrw {{{
let loaded_netrwPlugin = 0  " disable netrw
let g:netrw_banner = 0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+,\(^\|\s\s\)ntuser\.\S\+'
autocmd FileType netrw set nolist
" }}}
" }}}
" General {{{
" Disabled General {{{
"Plug 'vifm/vifm.vim'
"Plug 'fcpg/vim-waikiki' " {{{
"let maplocalleader = "-"
"let g:waikiki_default_maps = 1
"let g:waikiki_roots = [expand('~/notes/')]
" }}}
"Plug 'Jorengarenar/vim-syntaxMarkerFold' " {{{
"let g:syntaxMarkerFold_maxlevel = 2
" }}}
"Plug 'Xuyuanp/nerdtree-git-plugin' " {{{
"let g:NERDTreeIndicatorMapCustom = {
"    \ "Modified"  : "✹",
"    \ "Staged"    : "✚",
"    \ "Untracked" : "✭",
"    \ "Renamed"   : "➜",
"    \ "Unmerged"  : "═",
"    \ "Deleted"   : "✖",
"    \ "Dirty"     : "✗",
"    \ "Clean"     : "✔︎",
"    \ 'Ignored'   : '☒',
"    \ "Unknown"   : "?"
"    \ }
"let g:NERDTreeShowIgnoredStatus = 0
" }}}
"Plug 'justinmk/vim-sneak' " {{{
"let g:sneak#label = 1
" }}}
"Plug 'lervag/wiki.vim' " {{{
"let g:wiki_root = expand("~/notes/")
" }}}
"Plug 'b4b4r07/vim-buftabs' " {{{
"let g:buftabs_enabled = 1
"let g:buftabs_in_statusline = 1
"let g:buftabs_in_cmdline = 0
"let g:buftabs_only_basename = 1
"let g:buftabs_active_highlight_group = "Visual"
"let g:buftabs_inactive_highlight_group = ""
"let g:buftabs_statusline_highlight_group = ""
"let g:buftabs_marker_start = "["
"let g:buftabs_marker_end = "]"
"let g:buftabs_separator = "-"
"let g:buftabs_marker_modified = "!"
" }}}
"plug 'zefei/vim-wintabs' " {{{
" commands             | mapping keys                 | replacing Vim commands
" ---------------------+------------------------------+-----------------------
" :WintabsNext         | <Plug>(wintabs_next)         | :bnext!
" :WintabsPrevious     | <Plug>(wintabs_previous)     | :bprevious!
" :WintabsClose        | <Plug>(wintabs_close)        | :bdelete
" :WintabsUndo         | <Plug>(wintabs_undo)         |
" :WintabsOnly         | <Plug>(wintabs_only)         |
" :WintabsCloseWindow  | <Plug>(wintabs_close_window) | :close, CTRL-W c
" :WintabsOnlyWindow   | <Plug>(wintabs_only_window)  | :only, CTRL-W o
" :WintabsCloseVimtab  | <Plug>(wintabs_close_vimtab) | :tabclose
" :WintabsOnlyVimtab   | <Plug>(wintabs_only_vimtab)  | :tabonly
" :help wintabs-commands

"map <C-H> <Plug>(wintabs_previous)
"map <C-L> <Plug>(wintabs_next)
"map <C-T>c <Plug>(wintabs_close)
"map <C-T>u <Plug>(wintabs_undo)
"map <C-T>o <Plug>(wintabs_only)
"map <C-W>c <Plug>(wintabs_close_window)
"map <C-W>o <Plug>(wintabs_only_window)
"command! Tabc WintabsCloseVimtab
"command! Tabo WintabsOnlyVimtab
" }}}
"Plug 'bagrat/vim-buffet' " {{{
"noremap <Tab> :bn<CR>
"noremap <S-Tab> :bp<CR>
"noremap <Leader><Tab> :Bw<CR>
"noremap <Leader><S-Tab> :Bw!<CR>
"noremap <C-t> :tabnew split<CR>
"nmap <leader>1 <Plug>BuffetSwitch(1)
"nmap <leader>2 <Plug>BuffetSwitch(2)
"nmap <leader>3 <Plug>BuffetSwitch(3)
"nmap <leader>4 <Plug>BuffetSwitch(4)
"nmap <leader>5 <Plug>BuffetSwitch(5)
"nmap <leader>6 <Plug>BuffetSwitch(6)
"nmap <leader>7 <Plug>BuffetSwitch(7)
"nmap <leader>8 <Plug>BuffetSwitch(8)
"nmap <leader>9 <Plug>BuffetSwitch(9)
"nmap <leader>0 <Plug>BuffetSwitch(10)
"let g:buffet_always_show_tabline = 1
"let g:buffet_powerline_separators = 0
"let g:buffet_separator = ""
"let g:buffet_show_index = 0
"let g:buffet_max_plug = 10
"let g:buffet_use_devicons = 1
"let g:buffet_tab_icon = "#"
"let g:buffet_new_buffer_name = "*"
"let g:buffet_modified_icon = "+"
"let g:buffet_left_trunc_icon = "<"
"let g:buffet_right_trunc_icon = ">"
"function! g:BuffetSetCustomColors()
"  highlight! BuffetCurrentBuffer cterm=NONE ctermbg=5 ctermfg=8 guibg=#00FF00 guifg=#000000
"endfunction
" }}}
"Plug 'voldikss/vim-codelf', { 'on': 'Codelf' } " {{{
"inoremap <silent> <leader>ce <C-R>=codelf#start()<CR>
"nnoremap <silent> <leader>ce :call codelf#start()<CR>
"let g:codelf_enable_popup_menu = v:true
" }}}
"Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } " {{{
"Plug 'junegunn/fzf.vim'
"nmap <leader>f :Files<CR>
"if !PlugLoaded('ctrlp.vim')
"  nmap <C-p> :GFiles<CR>
"endif
"
"" Always enable preview window on the right with 60% width
"let g:fzf_preview_window = 'right:60%'
"
"" Simple preview for :Files with only cat
""command! -bang -nargs=? -complete=dir Files
""    \ call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview', 'cat {}']}, <bang>0)
""command! -bang -nargs=? -complete=dir Files
""    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--layout=reverse', '--info=inline']}), <bang>0)
"command! -bang -nargs=? -complete=dir Files
"    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
"
"" fzf#vim#marks({'options': ['--preview', 'echo line = {}']})
"" '--preview', 'cat -n {-1} | egrep --color=always -C 10 ^[[:space:]]*{2}[[:space:]]'
" }}}
"Plug 'junegunn/limelight.vim', { 'on': 'Limelight' } " {{{
" " Color name (:help cterm-colors) or ANSI code
"let g:limelight_conceal_ctermfg = 'gray'
"let g:limelight_conceal_ctermfg = 240
"" Color name (:help gui-colors) or RGB color
"let g:limelight_conceal_guifg = 'DarkGray'
"let g:limelight_conceal_guifg = '#777777'
"" Default: 0.5
""let g:limelight_default_coefficient = 0.7
"" Number of preceding/following paragraphs to include (default: 0)
""let g:limelight_paragraph_span = 1
"" Beginning/end of paragraph
""   When there's no empty line between the paragraphs
""   and each paragraph starts with indentation
""let g:limelight_bop = '^\s'
""let g:limelight_eop = '\ze\n^\s'
"" Highlighting priority (default: 10)
""   Set it to -1 not to overrule hlsearch
""let g:limelight_priority = 10
" }}}
"if has('nvim') || has('patch-8.0.902') " {{{
"  Plug 'mhinz/vim-signify'
"else
"  Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
"default updatetime 4000ms is not good for async update
"set updatetime=100
"endif " }}}
"Plug 'SirVer/ultisnips' " {{{
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"let g:UltiSnipsEditSplit="vertical"
" }}}
"Plug 'camspiers/lens.vim' " {{{
"let g:lens#animate = 1
"let g:lens#disabled_filetypes = ['nerdtree', 'fzf']
"let g:lens#disabled = 0
" }}}
"Plug 'scrooloose/nerdcommenter' " {{{
"vmap <leader>cc <plug>NERDCommenterToggle
"nmap <leader>cc <plug>NERDCommenterToggle
" }}}
"Plug 'zhimsel/vim-stay " {{{
"set viewoptions=cursor,folds,slash,unix
" }}}
"Plug 'DrCracket/painless-digraph' " {{{
"map <silent> <Leader>de <Plug>(PainlessdigraphEnable)
"map <silent> <Leader>dd <Plug>(PainlessdigraphDisable)
"map <silent> <Leader>dt <Plug>(PainlessdigraphToggle)
" }}}
"Plug 'vim-syntastic/syntastic' " {{{
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
" }}}
"Plug 'tmhedberg/SimpylFold' " {{{
"let g:SimpylFold_docstring_preview = 1
"let g:SimpylFold_fold_docstring = 1
"let g:SimpylFold_fold_import = 1
" }}}
"Plug 'unblevable/quick-scope' " {{{
" " Trigger a highlight in the appropriate direction when pressing these keys:
"let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
"augroup qs_colors
"  autocmd!
"  autocmd ColorScheme * highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
"  autocmd ColorScheme * highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
"augroup END
"
"nmap <leader>tq <plug>(QuickScopeToggle)
"xmap <leader>tq <plug>(QuickScopeToggle)
"
"let g:qs_enable=1
"
"let g:qs_buftype_blacklist = ['terminal', 'nofile']
"
"let g:qs_max_chars=1000
"
"let g:qs_lazy_highlight = 0
" }}}
"Plug 'mhinz/vim-startify' " {{{
"let g:startify_session_dir = '~/.config/nvim/session'
"let g:startify_lists = [
"          \ { 'type': 'files',     'header': ['   Files']            },
"          \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
"          \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
"          \ { 'type': 'sessions',  'header': ['   Sessions']       },
"          \ ]
"let g:startify_bookmarks = [
"            \ { 'c': '~/.config/nvim/init.vim' },
"            \ { 'z': '~/.config/zsh/.zshrc' },
"            \ '~/Blog',
"            \ '~/Code',
"            \ '~/Pics',
"            \ ]
"let g:startify_session_autoload = 1
"let g:startify_session_delete_buffers = 1
"let g:startify_change_to_vcs_root = 0
"let g:startify_fortune_use_unicode = 1
"let g:startify_session_persistence = 1
"let g:startify_enable_special = 0
"let g:startify_custom_header = [
        "\ '   _  __     _         __  ___         __     ___ ',
        "\ '  / |/ /  __(_)_ _    /  |/  /__ _____/ /    |_  |',
        "\ ' /    / |/ / /  ` \  / /|_/ / _ `/ __/ _ \  / __/ ',
        "\ '/_/|_/|___/_/_/_/_/ /_/  /_/\_,_/\__/_//_/ /____/ ',
        "\]
" }}}
"Plug 'mbbill/undotree' " {{{
"map <silent> <C-u> :UndotreeToggle<CR>
" }}}
"Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
"Plug 'rlue/vim-barbaric'
"Plug 'justinmk/vim-dirvish'
"Plug 'kristijanhusak/vim-dirvish-git'
"Plug 'bounceme/remote-viewer'
"if has('nvim') || has('patch-8.0.902')
"  Plug 'mhinz/vim-signify'
"else
"  Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
"endif
"Plug 'ryanoasis/vim-devicons'
"Plug 'jlanzarotta/bufexplorer'
"Plug 'severin-lemaignan/vim-minimap'
"Plug 'matze/vim-tex-fold', { 'for': 'tex' }
"Plug 'matze/vim-ini-fold', { 'for': 'ini' }
"Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
"Plug 'mg979/vim-xtabline'
"RRethy/vim-illuminate
"Plug 'ctrlpvim/ctrlp.vim'
"Plug 'tpope/vim-fugitive'
"Plug 'tpope/vim-rhubarb'
"Plug 'junegunn/gv.vim'
"Plug 'airblade/vim-rooter'
"if $DISPLAY != ''
"  Plug 'ying17zi/vim-live-latex-preview', { 'for': 'tex' } " requires biber
"endif
"Plug 'chazy/dirsettings'
"if has('python3') && $DISPLAY != ''
"  Plug 'anned20/vimsence'
"endif
"Plug 'easymotion/vim-easymotion'
"Plug 'vimoutliner/vimoutliner'
"Plug 'michal-h21/vimwiki-sync'
"Plug 'michal-h21/vim-zettel'
"Plug 'Kody-Quintana/bspwm_border_color'
"Plug 'amerlyq/vim-focus-autocmd'
"Plug 'jceb/vim-orgmode'
"Plug 'editorconfig/editorconfig-vim'
"Plug 'liuchengxu/vim-clap'
"Plug 'tpope/vim-dispatch'
"Plug 'habamax/vim-asciidoctor'
"Plug 'pbrisbin/vim-mkdir'
"Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }
"Plug 'lifepillar/vim-cheat40', { 'on': 'Cheat40' }
"Plug 'inkarkat/vim-ingo-library', { 'branch': 'stable' }
"Plug 'inkarkat/vim-ModelineCommands', { 'branch': 'stable' }
"Plug 'tyru/open-browser.vim', { 'on': 'RunningX' }
"Plug 'lervag/wiki-ft.vim'
"Plug 'liuchengxu/vista.vim'
"Plug 'honza/vim-snippets'
"Plug 'Yggdroot/hiPairs'
"Plug 'tpope/vim-surround'
"Plug 'rstacruz/vim-closer'
"Plug 'tpope/vim-endwise'
"Plug 'segeljakt/vim-isotope'
"Plug 'nvie/vim-flake8'
"Plug 'fcpg/vim-waikiki'
"Plug 'svermeulen/vim-subversive'
"Plug 'svermeulen/vim-cutlass'
"Plug 'svermeulen/vim-yoink'
"Plug 'pedrohdz/vim-yaml-folds'
"Plug 'jceb/vim-orgmode'
"Plug 'justinmk/vim-dirvish'
"Plug 'jonrad/vim-hi-hue'
"plug 'zefei/vim-wintabs-powerline'
"Plug 'bling/vim-bufferline'
"Plug 'airblade/vim-gitgutter'
"Plug 'yuttie/comfortable-motion.vim'
"Plug 'psliwka/vim-smoothie'
"Plug 'tpope/vim-repeat'
"Plug 'svermeulen/vim-macrobatics'
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
"Plug 'mechatroner/rainbow_csv'
"Plug 'sheerun/vim-polyglot'
"Plug 'mbbill/undotree'
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
" }}}

Plug 'dstein64/vim-startuptime'
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
Plug 'lervag/wiki.vim' " {{{
" https://github.com/lervag/wiki.vim/blob/master/doc/wiki.txt
let g:wiki_root = expand('~/notes')
let g:wiki_filetypes = ['md']
let g:wiki_link_extension = '.md'
let g:wiki_link_target_type = 'md'
let g:wiki_list_todos = ['TODO', 'DONE']
" }}}
"Plug 'vimwiki/vimwiki' " {{{
"" https://opensource.com/article/18/6/vimwiki-gitlab-notes
"" https://blog.mague.com/?p=602
"let g:vimwiki_ext = '.md'
"let g:vimwiki_syntax = 'markdown'
"let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
"let g:vimwiki_root = expand('~/notes/')
"let g:vimwiki_list = [{'path':      expand('~/notes/'),
"                     \ 'path_html': expand('~/notes/') . '.www',
"                     \ 'ext':       '.md',
"                     \ 'syntax':    'markdown',
"                     \ 'auto_toc':  0,
"                     \ 'auto_tags': 0},
"                     \{'path':      expand('~/notes/') . 'work',
"                     \ 'path_html': expand('~/notes/') . 'work/.www',
"                     \ 'ext':       '.md',
"                     \ 'syntax':    'markdown',
"                     \ 'auto_toc':  0,
"                     \ 'auto_tags': 0}]
"
""autocmd BufRead,BufNewFile *.wiki set filetype=vimwiki
"autocmd FileType vimwiki map D :VimwikiMakeDiaryNote<CR>
"
""autocmd FileType vimwiki execute 'echo hi'
""augroup WikiSync | autocmd!
"  "autocmd FileType vimwiki :!~/notes/.ops/sync
"  "autocmd FileType vimwiki :!~/notes/.ops/sync
"  "autocmd FileType vimwiki :autocmd! WikiSync BufRead,BufNewFile,BufWrite :echo hi
"  "autocmd FileType BufRead,BufNewFile */somepath/** set filetype=sometype
""augroup END
"
"" Autosave anytime there are updates to write & user is inactive
""autocmd CursorHold * update
"
"" Allow "normal" editor style tab/shift-tab indent/dedent. (Only in vimwiki
"" buffers!)
"let g:vimwiki_table_mappings = 0
"autocmd FileType vimwiki silent! iunmap <buffer> <Tab>
"autocmd FileType vimwiki map <buffer> <Tab> <Plug>VimwikiIncreaseLvlSingleItem
"autocmd FileType vimwiki map <buffer> <S-Tab> <Plug>VimwikiDecreaseLvlSingleItem
"" Not only do we want regular edit-style tab, but we also want it to be useful
"" for completing the tag-based insert-completion when only one match is
"" displayed.
"" TODO: try making it useful even when >1 possible match has been narrowed down
"autocmd FileType vimwiki imap <buffer> <expr> <Tab> pumvisible()
"\    ? "\<C-N>\<C-Y>:"
"\    : "\<Plug>VimwikiIncreaseLvlSingleItem"
"autocmd FileType vimwiki imap <buffer> <S-Tab> <Plug>VimwikiDecreaseLvlSingleItem
"
"" Don't make temporary wikis out of other .md files tho...sheesh. (Among other
"" things, this trips an auto-cd behavior I don't usually want.)
"let g:vimwiki_global_ext = 0
"
"" Allow 'gx' to open URLs (this technically works anywhere, not just in
"" vimwiki) that include question marks, hashes etc. Note: cWORD not cword.
"let g:netrw_gx = "<cWORD>"
"
"" Override behavior of (not actual mapping for) hitting Return in list items.
"" Specifically, change the 1st number from 1 to 3: Return always inserts a new
"" line item. 2nd number (set to 5) stays same: Enter on empty list item deletes
"" the list item.
"" TODO: except...this is firing even when not in a list item (i.e. it makes new
"" indented list items when mashing enter on an already blank line)! Clearly the
"" default isn't doing that; figure out how.
""autocmd FileType vimwiki inoremap <buffer> <CR> <Esc>:VimwikiReturn 3 5<CR>
"
"" Always 'cd' to vimwiki directory when opening vimwiki pages. This means git,
"" search, etc should always 'just work'.
"" TODO: if I start using subdirs, see whether this goes to wiki root or not...
"let g:vimwiki_auto_chdir = 1
"
"" Prevent saving wiki files that didn't get tags. So easy to forget!
""function CheckForTags()
""    try
""        " Note: regexp here MUST be single-quoted to avoid slash problems
""        if getbufline("", 1)[0] !~ ':.\+:'
""            echoerr "Don't forget tags!!"
""        endif
""    endtry
""endfunction
""" TODO: make variable out of vimwiki root? used in 3x places now
""autocmd BufWritePre ~/vimwiki/*.md call CheckForTags()
"
"" Auto git commit/push on save for vimwiki files, every >=5min. Pauper's
"" Evernote sync.
"" TODO: this may or may not work w/ subdirs, forget exactly how pattern works
""autocmd BufWritePost ~/notes/*.md execute 'echo hi'
"
"
"" Try vimwiki folding in a manner compatible with markdown syntax
"" (NOTE: setting to 'list' works well enough for lists, but then does not work
"" for sections, so kinda gotta pick one or the other :()
"let g:vimwiki_folding = 'expr'

" https://stackoverflow.com/questions/40549332/how-to-check-if-ssh-agent-is-already-running-in-bash
" Run ssh-agent if it's not running {{{
"function! CallbackTest()
"  echomsg 'Callback executed successfully!'
"endfunction
"function! CheckSSHAgent(callback)
"  if !($SSH_AGENT_PID)
"    if confirm("SSH_AGENT_PID unset, try adding ssh-agent?", "&Yes\n&No", 2) != 1
"      return 2
"    endif
"  endif

"  silent !if test -z "${SSH_AGENT_PID+x}"; then
"  \   eval "$(ssh-agent)";
"  \ fi;
"  \ if test "$(ssh-add -L | grep id_rsa | wc -l)" -le 0; then
"  \   ssh-add;
"  \   exit 69;
"  \ fi

  "if v:shell_error == 69
  "  execute "call " . a:callback
  "endif
"endfunction
" }}}

" WikiSyncPull {{{
"function! WikiSyncPull()
"  silent execute "!git -C " . expand('~/wiki/') . " pull"
"endfunction
" }}}
" WikiSyncSave {{{
"function! WikiSyncSave()
"  if &ft =~ 'asciidoctor'
"    silent Asciidoctor2HTML
"   "silent execute '!mv ' . g:wikidir . '%:t ' . g:wikidir . 'html/'
"  elseif &ft =~ 'vimwiki'
"    silent VimwikiAll2HTML
"   "call vimwiki#html#WikiAll2HTML(g:wikidir)
"  endif
"  let g:has_modified = 1
"endfunction
" }}}
" WikiSyncPush {{{
"function! WikiSyncPush()
"  if exists('g:has_modified')
"    silent execute '!git -C ' . expand('~/wiki/') . ' pull'
"    silent execute '!git -C ' . expand('~/wiki/') . ' add -u'
"    silent execute '!git -C ' . expand('~/wiki/') . ' commit -m "Auto push of %:t at ' . strftime('%a-%FT%T%z') .'"'
"    execute '!git -C ' . expand('~/wiki/') . ' push origin master'
"    unlet g:has_modified
"
"    while true
"      if (system('git status')) =~ 'ahead of'
"        if confirm("Could not push changes, add ssh-agent and retry?", "&Yes\n&No", 1) == 1
"          !eval "$(ssh-agent)" && ssh-add
"          call WikiSyncPush()
"        else
"          break
"        endif
"      endif
"    endwhile
"  endif
"endfunction
" }}}

" augroup WikiSync {{{
"augroup WikiSync | autocmd!
"  autocmd FileType asciidoctor call WikiSyncPull()
"  autocmd FileType asciidoctor cabbrev wq write <bar> quit
"  autocmd FileType vimwiki call WikiSyncPull()
"  autocmd FileType vimwiki cabbrev wq write <bar> quit
"
"  autocmd BufWritePost *.adoc :call WikiSyncSave()
"  autocmd BufWritePost *.wiki :call WikiSyncSave()
"
"  autocmd BufWinLeave *.adoc :call WikiSyncPush()
"  autocmd BufWinLeave *.wiki :call WikiSyncPush()
"augroup END
" }}}
" }}}
Plug 'mattn/calendar-vim' " {{{
function! ToggleCalendar()
  if CalendarEnabled()
    execute ":CalendarVR"
    execute "q"
  else
    execute ":CalendarVR"
  endif
endfunction
map <silent> <C-b> :call ToggleCalendar() <bar> wincmd p<CR>
"autocmd FileType vimwiki map c :call ToggleCalendar()<CR>
"autocmd bufenter * if ((winnr("$") == 1 && (NerdTreeEnabled() || CalendarEnabled())) || (winnr("$") == 2 && (NerdTreeEnabled() && CalendarEnabled()))) | q | endif

"autocmd bufenter * if (
"    \ (winnr("$") == 1 && (exists("b:Calendar")) || (exists("b:NERDTree") && b:NERDTree.isTabTree()))
"    \ || (winnr("$") == 2 && (bufname("Calendar") && g:NERDTree.IsOpen()))
"  \ ) | q | endif

"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" }}}
Plug 'ap/vim-buftabline' " {{{
"set hidden
"buffer binds
" }}}
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } | Plug 'junegunn/fzf.vim' | Plug 'camspiers/animate.vim' " {{{
nmap <silent> <leader>f :FZF<CR>
let g:fzf_layout = {
 \ 'window': 'new | wincmd J | resize 1 | call animate#window_percent_height(0.5)'
\ }
" }}}
if has('python3') && executable('rg') | Plug 'alok/notational-fzf-vim' " {{{
  let g:nv_search_paths = [ expand('~/notes/') ]

  " String. Set to '' (the empty string) if you don't want an extension appended by default.
  " Don't forget the dot, unless you don't want one.
  let g:nv_default_extension = ''
  
  " String. Default is first directory found in `g:nv_search_paths`. Error thrown
  "if no directory found and g:nv_main_directory is not specified
  "let g:nv_main_directory = g:nv_main_directory or (first directory in g:nv_search_paths)
  
  " Dictionary with string keys and values. Must be in the form 'ctrl-KEY':
  " 'command' or 'alt-KEY' : 'command'. See examples below.
  let g:nv_keymap = {
                      \ 'ctrl-s': 'split ',
                      \ 'ctrl-v': 'vertical split ',
                      \ 'ctrl-t': 'tabedit ' }
  
  " String. Must be in the form 'ctrl-KEY' or 'alt-KEY'
  let g:nv_create_note_key = 'ctrl-x'
  
  " String. Controls how new note window is created.
  let g:nv_create_note_window = 'vertical split'
  
  " Boolean. Show preview. Set by default. Pressing Alt-p in FZF will toggle this for the current search.
  let g:nv_show_preview = 1
  
  " Boolean. Respect .*ignore files in or above nv_search_paths. Set by default.
  let g:nv_use_ignore_files = 1
  
  " Boolean. Include hidden files and folders in search. Disabled by default.
  let g:nv_include_hidden = 0
  
  " Boolean. Wrap text in preview window.
  let g:nv_wrap_preview_text = 1
  
  " String. Width of window as a percentage of screen's width.
  let g:nv_window_width = '40%'
  
  " String. Determines where the window is. Valid options are: 'right', 'left', 'up', 'down'.
  let g:nv_window_direction = 'down'
  
  " String. Command to open the window (e.g. `vertical` `aboveleft` `30new` `call my_function()`).
  "let g:nv_window_command = 'call my_function()'
  
  " Float. Width of preview window as a percentage of screen's width. 50% by default.
  let g:nv_preview_width = 50
  
  " String. Determines where the preview window is. Valid options are: 'right', 'left', 'up', 'down'.
  let g:nv_preview_direction = 'right'
  
  " String. Yanks the selected filenames to the default register.
  let g:nv_yank_key = 'ctrl-y'
  
  " String. Separator used between yanked filenames.
  let g:nv_yank_separator = "\n"
  
  " Boolean. If set, will truncate each path element to a single character. If
  " you have colons in your pathname, this will fail. Set by default.
  let g:nv_use_short_pathnames = 1
  
  "List of Strings. Shell glob patterns. Ignore all filenames that match any of
  " the patterns.
  let g:nv_ignore_pattern = ['summarize-*', 'misc*']
  
  " List of Strings. Key mappings like above in case you want to define your own
  " handler function. Most users won't want to set this to anything.
  let g:nv_expect_keys = []                                              
endif " }}}
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
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"let g:NERDTreeDirArrowExpandable = '>'
"let g:NERDTreeDirArrowCollapsible = 'v'

" Toggle NERDTree and focus editor
map <silent> <C-n> :NERDTreeToggle <bar> wincmd p<CR>

let NERDTreeAutoDeleteBuffer = 1
let NERDTreeQuitOnOpen = 1
let NERDTreeMinimalUI=1
let NERDTreeDirArrows = 1
let g:NERDTreeGitStatusWithFlags = 1
let NERDTreeShowHidden=1
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
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-html',
  \ 'coc-json',
  \ 'coc-vetur',
  \ 'coc-css',
  \ 'coc-yaml',
  \ 'coc-highlight',
  \ 'coc-markdownlint',
  \ 'coc-emoji'
  \ ]
  " \ 'coc-python',
"exec "CocInstall -sync " . join(get(g:, 'coc_global_extensions', []))
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
Plug 'skywind3000/vim-auto-popmenu', { 'for': 'markdown' } " {{{
let g:apc_enable_ft = {'text':1, 'markdown':1}
set cpt=.,k,w,b
set completeopt=menu,menuone,noselect
" }}}
" }}}
" Syntax highlighting {{{
" Disabled Syntax highlighting {{{
"Plug 'dense-analysis/ale'
"Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
"Plug 'mboughaba/i3config.vim', { 'for': 'conf' }
"Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'jinja' }
"Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
"Plug 'autozimu/LanguageClient-neovim'
" }}}

Plug 'chr4/nginx.vim', { 'for': 'nginx' }
Plug 'storyn26383/vim-vue', { 'for': 'vue' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
Plug 'kovetskiy/sxhkd-vim', { 'for': 'sxhkdrc' }
" }}}
" Colorschemes {{{
" Disabled Colorschemes {{{
"Plug 'AlessandroYorba/Sierra' " {{{
"let g:sierra_Sunset = 1
"let g:sierra_Twilight = 1
"let g:sierra_Midnight = 1
"let g:sierra_Pitch = 1
" }}}
"Plug 'ayu-theme/ayu-vim'
"Plug 'chriskempson/base16-vim'
"Plug 'sainnhe/edge'
"Plug 'tomasr/molokai'
"Plug 'AlessandroYorba/Despacio'
"Plug 'nightsense/snow'
"Plug 'nightsense/stellarized'
"Plug 'junegunn/seoul256.vim'
"Plug 'sjl/badwolf'
"Plug 'xero/sourcerer.vim'
"Plug 'mhartington/oceanic-next'
"Plug 'jacoborus/tender.vim'
"Plug 'joshdick/onedark.vim'
"Plug 'kaicataldo/material.vim'
"Plug 'flrnd/plastic.vim'
" }}}

Plug 'arcticicestudio/nord-vim'
Plug 'rakr/vim-one' " {{{
let g:one_allow_italics = 1
" }}}
" }}}
call plug#end()
endif

" }}}======================
" A E S T H E T I C S {{{
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
set ttimeout
set ttimeoutlen=100
set timeoutlen=500
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
set hidden " Allow switching buffers without writing
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
"inoremap <expr> <leader><leader> exists('b:texmap') ? '<Esc>/<++><Enter>"_c4l<Esc>\:unlet b\:texmap' : '<leader><leader>'

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

" qq to record, Q to replay
nnoremap Q @q

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
" Automatically close tab/vim if the only open buffers are NERDTree and/or Calendar {{{
function! TabQuit()
  " https://vim.fandom.com/wiki/Tabclose_instead_of_quit-all
  try
    if tabpagenr('$') > 1
      silent exec 'tabclose'
    else
      silent exec 'qa'
    endif
  catch
    echohl ErrorMsg | echo v:exception | echohl NONE
  endtry
endfunction
function! NerdTreeEnabled()
  return exists('t:NERDTreeBufName') && bufwinnr(t:NERDTreeBufName) != -1
endfunction
function! CalendarEnabled()
  return bufwinnr("Calendar") != -1
endfunction
function! ShouldClose()
  return (winnr("$") == 1 && (NerdTreeEnabled() || CalendarEnabled())) || (winnr("$") == 2 && (NerdTreeEnabled() && CalendarEnabled()))
endfunction
autocmd BufEnter * if ShouldClose() | call TabQuit() | endif
" }}}

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

function! s:OpenAnimatedLF() abort " {{{
  " Open lf in a terminal
  new term://lf
  " Send window to bottom and start with small height
  wincmd J | resize 1
  " Animate height to 66%
  call animate#window_percent_height(0.66)
endfunction
command! LF call s:OpenAnimatedLF()
" }}}
" }}}======================
