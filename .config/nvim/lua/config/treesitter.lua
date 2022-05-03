local ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not ok then return end

treesitter.setup({
  ensure_installed = {
    "bash",
    "bibtex",
    "c",
    "cmake",
    "comment",
    "commonlisp",
    "cooklang",
    "cpp",
    "css",
    "dart",
    "dockerfile",
    "go",
    "graphql",
    "haskell",
    "hcl",
    "help",
    "html",
    "http",
    "java",
    "javascript",
    "jsdoc",
    "jsonc",
    "latex",
    "lua",
    "make",
    "markdown",
    "nix",
    "python",
    "regex",
    "ruby",
    "rust",
    "scss",
    "toml",
    "tsx",
    "typescript",
    "vim",
    "vue",
    "yaml",
  },
  highlight = {
    enable = true,
    use_languagetree = true,
    additional_vim_regex_highlighting = false,
  },
  autopairs = {
    enable = true,
  },
  indent = {
    enable = true,
    disable = {
      "yaml",
    },
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = nil,
  }
})
