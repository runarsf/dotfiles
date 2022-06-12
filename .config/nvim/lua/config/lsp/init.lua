local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
  return
end

lspconfig.pyright.setup {}
lspconfig.gopls.setup {}

require "config.lsp.configs"
require("config.lsp.handlers").setup()
require "config.lsp.null-ls"
