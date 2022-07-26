local ok, prettyfold = pcall(require, "pretty-fold")
if not ok then return end

-- local ok, prettyfold_preview = pcall(require, 'pretty-fold.preview')
-- if ok then prettyfold_preview.setup() end

prettyfold.setup({
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
