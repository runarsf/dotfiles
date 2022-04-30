local ok, plugin = pcall(require, "bufferline")
if not ok then return end

-- TODO Tab groups
plugin.setup({
  options = {
    mode = "buffers",
    buffer_close_icon = "",
  }
})