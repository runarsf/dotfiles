vim.cmd [[
  " Get rid of that pesky command-line-window (:h command-line-window)
  autocmd CmdwinEnter * q

  " Return to last edit position when opening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif

  " Check if we need to reload the file when it changed
  autocmd FocusGained * :checktime

  " Save and restore code folding - https://vi.stackexchange.com/a/13874
  augroup AutoSaveGroup
  autocmd!
    " autocmd BufWinLeave *.* mkview
    " autocmd BufWinEnter *.* silent! loadview
    autocmd BufWinLeave,BufLeave,BufWritePost,BufHidden,QuitPre ?* nested silent! mkview!
    autocmd BufWinEnter ?* silent! loadview
  augroup end

  " Only show cursorline in active window
  augroup _cursorline
    autocmd!
    autocmd InsertLeave,WinEnter * set cursorline
    autocmd InsertEnter,WinLeave * set nocursorline
  augroup end

  " Disable automatic commenting on newline
  autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
]]
