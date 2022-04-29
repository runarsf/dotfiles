local Pkg = {}

-- Clean plugins and Packer state:
-- $ rm -rf ~/.local/share/nvim/site/pack/packer/opt/* ~/.local/share/nvim/site/pack/packer/start/* ~/.config/nvim/plugin/packer_compiled.lua

-- NOTE 'opt/' vs 'start/'
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  Pkg.Bootstrap = true
end

Pkg.bootstrap = function()
  -- Automatically install Packer
  if Pkg.Bootstrap then
    vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd("packadd packer.nvim")
    print("Installed Packer, please reopen Neovim...")
  end

  -- Automatically update Packer when updating plugins.lua
  vim.cmd([[
    augroup packer_user_config
      autocmd!
      autocmd BufWritePost plugins.lua source <afile> | PackerSync " => `PackerUpdate` -> `PackerCompile`
    augroup end
  ]])
end

-- Emulate python-like inner functions
Pkg.wrap = function(use)
  return function(spec)
    use(spec)
  end
end

Pkg.update = function()
  local ok, packer = pcall(require, "packer")
  if not ok then
    return
  end
  packer.sync()
end

Pkg.config = {
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

Pkg.setup = function(plugins, config)
  local cfg = vim.tbl_deep_extend("force", Pkg.config, config or {})

  Pkg.bootstrap()

  -- Use a protected call so we don't error out on first use
  local ok, packer = pcall(require, "packer")
  if not ok then
    return
  end

  packer.init(cfg)
  return packer.startup({
    function(use)
      use = Pkg.wrap(use)
      plugins(use)
    end,
  })
end

return Pkg