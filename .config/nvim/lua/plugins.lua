local pkg = require("util.pkg")

local plugins = function(use)
  use({ "wbthomason/packer.nvim", opt=true }) -- Have packer manage itself

  -- Utility --
  use({ "nvim-lua/popup.nvim" })              -- Popup API from Vim in Neovim
  use({ "nvim-lua/plenary.nvim" })            -- Useful lua functions
  use({ "antoinemadec/FixCursorHold.nvim" })  -- Fixes lsp doc highlight
  use({ "lewis6991/impatient.nvim" })         -- Speed up Lua module loading

  -- Themes --
  use({ "Shatur/neovim-ayu",
    config = function()
      require("config.theme").scheme("ayu-dark")
    end,
    -- "folke/tokyonight.nvim",
  })

  use({ "kyazdani42/nvim-web-devicons",
    opt = true,
    module = "nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup({ default=true })
    end,
  })

  use({ "kyazdani42/nvim-tree.lua",
    cmd = { "NvimTreeToggle", "NvimTreeOpen", "NvimTreeClose" },
    requires = { "nvim-web-devicons" },
    after = { "nvim-web-devicons" },
    config = function()
      require("config.nvim-tree")
    end,
  })

  use({ "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    -- opt = true,
    -- event = "BufRead",
    module = "treesitter",
    config = function()
      require("config.treesitter")
    end,
  })

  use({ "lukas-reineke/indent-blankline.nvim",
    -- requires = { "treesitter" },
    -- after = { "treesitter" },
    config = function()
      require("config.indent-blankline")
    end,
  })

  if pkg.Bootstrap then
    pkg.update()
  end
end

return pkg.setup(plugins)