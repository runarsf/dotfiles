local cfg = require("util")

vim.opt.timeoutlen = 300 -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.ttimeoutlen = 50 -- affects escape from insert mode

vim.g.mapleader = ","
vim.g.maplocalleader = " "

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
cfg.map("n", "n", "'Nn'[v:searchforward]", { expr=true })
cfg.map("n", "N", "'nN'[v:searchforward]", { expr=true })
cfg.map("x", "n", "'Nn'[v:searchforward]", { expr=true })
cfg.map("x", "N", "'nN'[v:searchforward]", { expr=true })
cfg.map("o", "n", "'Nn'[v:searchforward]", { expr=true })
cfg.map("o", "N", "'nN'[v:searchforward]", { expr=true })

-- Add undo break-points
cfg.map("i", ",", ",<c-g>u")
cfg.map("i", ".", ".<c-g>u")
cfg.map("i", ";", ";<c-g>u")

-- Better indents
cfg.map("v", "<", "<gv")
cfg.map("v", ">", ">gv")

-- Better tab movement
cfg.map("n", "<C-t>", ":tabnew<CR>")
--map("n", "<S-l>", ":bnext<CR>")
--map("n", "<S-h>", ":bprevious<CR>")
cfg.map("n", "H", "gT")
cfg.map("n", "L", "gt")

-- Better window navigation
cfg.map("n", "<C-h>", ":wincmd h<CR>")
cfg.map("n", "<C-j>", ":wincmd j<CR>")
cfg.map("n", "<C-k>", ":wincmd k<CR>")
cfg.map("n", "<C-l>", ":wincmd l<CR>")

cfg.map("n", "<C-Left>", "<C-w>h")
cfg.map("n", "<C-Down>", "<C-w>j")
cfg.map("n", "<C-Up>", "<C-w>k")
cfg.map("n", "<C-Right>", "<C-w>l")

cfg.map("", "<esc>", ":nohlsearch<CR>")
cfg.map("n", "<leader><space>", ":nohlsearch<CR>")
cfg.map("n", "<leader>w", ":w<CR>")
cfg.map("n", "<leader>q", ":q<CR>")
cfg.map("n", "<leader>wq", ":wq<CR>")
cfg.map("n", "<leader>..", ":messages<CR>")
cfg.map("n", "<leader>l", ":set cursorline!<CR>")
-- util.nnoremap("<leader>ll", ":set cursorcolumn!<CR>")

cfg.map("i", "jk", "<ESC>")
cfg.map("i", "#dn", ">/dev/null<space>2>&1")
-- keymap("i", "<expr> #!!", '"#!/usr/bin/env" . (empty(&filetype) ? "" : " ".&filetype)', opts)

-- Better terminal navigation
cfg.map("t", "<C-h>", "<C-\\><C-N><C-w>h")
cfg.map("t", "<C-j>", "<C-\\><C-N><C-w>j")
cfg.map("t", "<C-k>", "<C-\\><C-N><C-w>k")
cfg.map("t", "<C-l>", "<C-\\><C-N><C-w>l")

-- nvim-tree
cfg.map("n", "<C-n>", ":NvimTreeToggle<CR>")
cfg.map("v", "<C-n>", "<ESC>:NvimTreeToggle<CR>")
cfg.map("i", "<C-n>", "<ESC>:NvimTreeToggle<CR>")

function ToggleNumbers()
  local g = vim.g
  local w = vim.wo
  g.def_number         = g.def_number         ~= nil and true  or g.def_number
  g.def_relativenumber = g.def_relativenumber ~= nil and true  or g.def_relativenumber
  g.def_wrap           = g.def_wrap           ~= nil and false or g.def_wrap
  g.def_signcolumn     = g.def_signcolumn     ~= nil and "yes" or g.def_signcolumn

  local int_to_bool = function(int)
    if     int == 0 then return false
    elseif int == 1 then return true end
    return int
  end

  if w.number or w.relativenumber then
    g.def_number         = int_to_bool(w.number)
    g.def_relativenumber = int_to_bool(w.relativenumber)
    g.def_wrap           = int_to_bool(w.wrap)
    g.def_signcolumn     =             w.signcolumn

    w.number         = false
    w.relativenumber = false
    w.wrap           = false
    w.signcolumn     = "no"
  else
    w.number         = g.def_number
    w.relativenumber = g.def_relativenumber
    w.wrap           = g.def_wrap
    w.signcolumn     = g.def_signcolumn
  end
end
cfg.map("n", "<leader>n", "<cmd>lua ToggleNumbers()<CR>")