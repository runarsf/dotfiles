local cfg = require("util")

local ok, bufferline = pcall(require, "bufferline")
if not ok then return end

-- TODO Tab groups
bufferline.setup({
  options = {
    mode = "buffers",
    buffer_close_icon = "",
    close_command = "Bdelete! %d",
    close_icon = "",
    offsets = {{filetype = "NvimTree", text = "Files"}},
  },
})

-- cfg.map("n", "<leader>q", ":Bdelete! %d<CR>")
