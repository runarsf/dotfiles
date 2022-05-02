local cfg = require("util")
local ok, toggleterm = pcall(require, "toggleterm")
if not ok then return end

-- requires `hidden` to be set
toggleterm.setup({
  open_mapping = "<M-n>",
  size = 20,
  shading_factor = 2,
  direction = "float",
  float_opts = {
    border = "curved",
    winblend = 0,
    highlights = {
      border = "Normal",
      background = "Normal",
    },
  },
})

cfg.map("t", "<ESC><ESC>", "<C-\\><C-N>")

local Terminal = require('toggleterm.terminal').Terminal

Glow = function()
  local file = vim.fn.expand("%:p") -- current file
  if file:sub(-#'.md') ~= '.md' then
    file = vim.fn.expand("%:p:h") -- current directory
  end
  local glow = Terminal:new({ cmd="PAGER='less -r' glow -s dark -p "..file, hidden=false })
  glow:toggle()
end

-- vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua Glow()<CR>", {noremap = true, silent = true})
cfg.map("n", "<leader>g", "<CMD>lua Glow()<CR>")
