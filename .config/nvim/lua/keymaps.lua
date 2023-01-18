local cfg = require("util")

vim.opt.timeoutlen = 300 -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.ttimeoutlen = 50 -- affects escape from insert mode

-- NOTE All leader maps go in ./config/which-key.lua
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
cfg.map("n", "<C-t>", "<CMD>tabnew<CR>")
cfg.map("n", "<S-l>", "<CMD>tabnext<CR>")
cfg.map("n", "<S-h>", "<CMD>tabprevious<CR>")
-- cfg.map("n", "H", "gT")
-- cfg.map("n", "L", "gt")

-- Better window navigation
cfg.map("n", "<C-h>", "<CMD>wincmd h<CR>")
cfg.map("n", "<C-j>", "<CMD>wincmd j<CR>")
cfg.map("n", "<C-k>", "<CMD>wincmd k<CR>")
cfg.map("n", "<C-l>", "<CMD>wincmd l<CR>")

cfg.map("n", "<C-Left>", "<C-w>h")
cfg.map("n", "<C-Down>", "<C-w>j")
cfg.map("n", "<C-Up>", "<C-w>k")
cfg.map("n", "<C-Right>", "<C-w>l")

cfg.map("n", "<ESC><ESC>", "<CMD>nohlsearch<CR>")

cfg.map("i", "jk", "<ESC>")
cfg.map("i", "#dn", ">/dev/null<space>2>&1")
-- vim.cmd("cnoreabbrev <expr> #!!", '"#!/usr/bin/env" . (empty(&filetype) ? "" : " ".&filetype)', opts)

-- Better terminal navigation
cfg.map("t", "<C-h>", "<C-\\><C-N><C-w>h")
cfg.map("t", "<C-j>", "<C-\\><C-N><C-w>j")
cfg.map("t", "<C-k>", "<C-\\><C-N><C-w>k")
cfg.map("t", "<C-l>", "<C-\\><C-N><C-w>l")

-- nvim-tree
cfg.map("n", "<C-n>", "<CMD>NvimTreeToggle<CR>")
cfg.map("v", "<C-n>", "<ESC><CMD>NvimTreeToggle<CR>")
cfg.map("i", "<C-n>", "<ESC><CMD>NvimTreeToggle<CR>")

vim.cmd("cnoreabbrev q1 q!")
