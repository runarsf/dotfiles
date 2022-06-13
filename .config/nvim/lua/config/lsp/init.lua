local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
  return
end

lspconfig.pyright.setup {}
lspconfig.rust_analyzer.setup {}
-- lspconfig.gopls.setup {}
-- lspconfig.vls.setup {}

require "config.lsp.configs"
require("config.lsp.handlers").setup()
require "config.lsp.null-ls"
