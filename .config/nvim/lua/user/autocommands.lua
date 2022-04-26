vim.cmd [[
  augroup _general_settings
    autocmd!
    autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR> 
    autocmd TextYankPost * silent!lua require('vim.highlight').on_yank({higroup = 'Visual', timeout = 200}) 
    autocmd BufWinEnter * :set formatoptions-=cro
    autocmd FileType qf set nobuflisted
  augroup end

  augroup _git
    autocmd!
    autocmd FileType gitcommit setlocal wrap
    autocmd FileType gitcommit setlocal spell
  augroup end

  augroup _markdown
    autocmd!
    autocmd FileType markdown setlocal wrap
    autocmd FileType markdown setlocal spell
  augroup end

  augroup _auto_resize
    autocmd!
    autocmd VimResized * tabdo wincmd = 
  augroup end

  augroup _alpha
    autocmd!
    autocmd User AlphaReady set showtabline=0 | autocmd BufUnload <buffer> set showtabline=2
  augroup end

  augroup _fern
    autocmd!
    autocmd StdinReadPre * let s:std_in=1
    " No argument was specified
    autocmd VimEnter * ++nested if !argc() && !exists("s:std_in") | silent Fern -drawer -width=25 -reveal=% . | wincmd p | endif
    " Specified argument is a directory
    autocmd VimEnter * ++nested if isdirectory(expand('<afile>')) && !exists("s:std_in") | silent Fern -reveal=% .| endif
  augroup end

  " Return to last edit position when opening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

  " Disable automatic commenting on new lines
  " autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

  augroup _fold
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
  augroup end
]]

-- Autoformat
-- augroup _lsp
--   autocmd!
--   autocmd BufWritePre * lua vim.lsp.buf.formatting()
-- augroup end
