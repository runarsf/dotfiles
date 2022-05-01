local ok, treesitter = pcall(require, "nvim-treesitter.configs")
if not ok then return end

treesitter.setup({
  ensure_installed = {
    "bash",
    "c",
    "cmake",
    "comment",
    "cpp",
    "css",
    "go",
    "html",
    "javascript",
    "jsonc",
    "markdown",
    "latex",
    "bibtex",
    "lua",
    "python",
    "regex",
    "rust",
    "toml",
    "tsx",
    "typescript",
    "vue",
    "vim",
    "yaml",
    "scss",
    "nix",
    "http",
    "dockerfile",
    "cooklang",
  },
  highlight = {
    enable = true,
    use_languagetree = true,
    additional_vim_regex_highlighting = true,
  },
  autopairs = {
    enable = true,
  },
  indent = {
    enable = false,
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
})