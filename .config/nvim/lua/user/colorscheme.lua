-- vim.cmd [[
-- try
--   colorscheme ayu-dark
-- catch /^Vim\%((\a\+)\)\=:E185/
--   colorscheme default
--   set background=dark
-- endtry
-- ]]

local colorscheme = "ayu-dark"

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  vim.cmd [[
    colorscheme default
    set background=dark
  ]]
end

require "user.ayu"
