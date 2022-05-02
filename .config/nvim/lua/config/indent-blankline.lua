local ok, indentblankline = pcall(require, "indent_blankline")
if not ok then return end

vim.opt.list = true
vim.opt.listchars:append("trail:·")
vim.opt.listchars:append("nbsp:⎵")
vim.opt.listchars:append("tab:┊»")
-- eol:⏎

-- https://github.com/lukas-reineke/indent-blankline.nvim/blob/master/lua/indent_blankline/init.lua
indentblankline.setup({
  -- for example, context is off by default, use this to turn it on
  show_current_context = true,
  show_current_context_start = true,
  char = "▏", -- │▏▎
  buftype_exclude = { "terminal", "nofile" },
  use_treesitter = true,
  show_first_indent_level = true,
  show_trailing_blankline_indent = false,
  filetype_exclude = {
    "help",
    "startify",
    "dashboard",
    "packer",
    "neogitstatus",
    "NvimTree",
    "Trouble",
  },
  context_patterns = {
    "class",
    "return",
    "function",
    "method",
    "^if",
    "^while",
    "jsx_element",
    "^for",
    "^object",
    "^table",
    "block",
    "arguments",
    "if_statement",
    "else_clause",
    "jsx_element",
    "jsx_self_closing_element",
    "try_statement",
    "catch_clause",
    "import_statement",
    "operation_type",
  },
})

-- HACK: work-around for https://github.com/lukas-reineke/indent-blankline.nvim/issues/59
vim.wo.colorcolumn = "99999"
