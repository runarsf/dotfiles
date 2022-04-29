vim.cmd [[
  " Get rid of that pesky command-line-window (:h command-line-window)
  autocmd CmdwinEnter * q

  " Return to last edit position when opening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif

  " Check if we need to reload the file when it changed
  autocmd FocusGained * :checktime

  " Save and restore code folding
  augroup _fold
    autocmd!
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent! loadview
  augroup end

  " Only show cursorline in active window
  augroup _cursorline
    autocmd!
    autocmd InsertLeave,WinEnter * set cursorline
    autocmd InsertEnter,WinLeave * set nocursorline
  augroup end
]]