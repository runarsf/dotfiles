local cfg = require("util")
local ok, nvimtree = pcall(require, "nvim-tree")
if not ok then return end

nvimtree.setup({
  disable_netrw = true,
  ignore_ft_on_setup = {
    "startify",
    "dashboard",
    "alpha",
  },
  hijack_unnamed_buffer_when_opening = true,
  update_focused_file = {
    enable = true,
    update_cwd = true,
  },
  update_cwd = true,
  filters = {
    custom = { ".git", "node_modules", ".cargo" },
  },
  actions = {
    open_file = {
      quit_on_open = true,
      window_picker = {
        enable = false,
      },
    },
  },
})

-- These need to be mapped somewhere else if nvim-tree is lazy-loaded
-- cfg.map("n", "<C-n>", ":NvimTreeToggle<CR>")
-- cfg.map("v", "<C-n>", "<ESC>:NvimTreeToggle<CR>")
-- cfg.map("i", "<C-n>", "<ESC>:NvimTreeToggle<CR>")

-- vim.cmd [[
--   augroup _nvim_tree
--     autocmd!
--     " autocmd StdinReadPre * let s:std_in=1
--     " No argument was specified
--     " autocmd VimEnter * ++nested if !argc() && !exists("s:std_in") | NvimTreeOpen | endif
--     " Specified argument is a directory
--     " autocmd VimEnter * ++nested if isdirectory(expand('<afile>')) && !exists("s:std_in") | NvimTreeOpen | endif
--     " Auto close if last window left
--     " autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif
--   augroup end
-- ]]