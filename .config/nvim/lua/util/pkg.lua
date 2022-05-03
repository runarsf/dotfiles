local M = {}

-- Clean plugins and Packer state:
-- $ rm -rf ~/.local/share/nvim/site/pack/packer/opt/* ~/.local/share/nvim/site/pack/packer/start/* ~/.config/nvim/plugin/packer_compiled.lua

-- NOTE 'opt/' vs 'start/'
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  M.Bootstrap = true
end

M.bootstrap = function()
  -- Automatically install Packer
  if M.Bootstrap then
    vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd("packadd packer.nvim")
    print("Installed Packer, please reopen Neovim after plugins are done with the initial setup...")
  end

  -- Automatically update Packer when updating plugins.lua
  vim.cmd([[
    augroup packer_user_config
      autocmd!
      autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
  ]])
end

-- Emulate python-like inner functions
M.wrap = function(use)
  return function(spec)
    use(spec)
  end
end

M.update = function()
  local ok, packer = pcall(require, "packer")
  if not ok then
    return
  end
  packer.sync()
end

M.config = {
  profile = {
    enable = true,
    threshold = 0, -- the amount in ms that a plugins load time must be over for it to be included in the profile
  },
  display = {
    open_fn = function()
      return require("packer.util").float({ border="rounded" })
    end,
  },
}

M.setup = function(plugins, config)
  local cfg = vim.tbl_deep_extend("force", M.config, config or {})

  M.bootstrap()

  -- Use a protected call so we don't error out on first use
  local ok, packer = pcall(require, "packer")
  if not ok then
    return
  end

  packer.init(cfg)
  return packer.startup({
    function(use)
      use = M.wrap(use)
      plugins(use)
    end,
  })
end

return M
