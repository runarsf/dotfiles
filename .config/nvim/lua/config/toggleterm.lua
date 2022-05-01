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