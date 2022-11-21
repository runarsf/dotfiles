local ok, todo = pcall(require, "todo-comments")
if not ok then return end

todo.setup({
  highlight = {
    pattern = [[.*<(KEYWORDS)\s*:*]],
  },
  search = {
    pattern = [[\s\b(KEYWORDS)\b\s]]
  }
})
