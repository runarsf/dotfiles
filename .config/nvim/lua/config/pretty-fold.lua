local ok, plugin = pcall(require, "pretty-fold")
if not ok then return end

require('pretty-fold.preview').setup()

plugin.setup({
  keep_indentation = false,
  fill_char = ' ',
  sections = {
    left = {
      '»', 'content', function() return string.rep('›', vim.v.foldlevel - 1) end
    },
    right = {
      '{ ', 'number_of_folded_lines', ' }  ',
    }
  }
})