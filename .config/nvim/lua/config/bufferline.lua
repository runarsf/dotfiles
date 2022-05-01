local ok, bufferline = pcall(require, "bufferline")
if not ok then return end

-- TODO Tab groups
bufferline.setup({
  options = {
    mode = "tabs",
    buffer_close_icon = "",
  }
})