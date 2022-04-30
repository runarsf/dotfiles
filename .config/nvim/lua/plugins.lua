local pkg = require("util.pkg")

local plugins = function(use)
  use({ "wbthomason/packer.nvim", opt=true }) -- Have packer manage itself

  -- Utility --
  use({ "nvim-lua/popup.nvim" })              -- Popup API from Vim in Neovim
  use({ "nvim-lua/plenary.nvim" })            -- Useful lua functions
  use({ "antoinemadec/FixCursorHold.nvim" })  -- Fixes lsp doc highlight
  use({ "lewis6991/impatient.nvim" })         -- Speed up Lua module loading

  use({ -- Themes
    "Shatur/neovim-ayu",
    -- "folke/tokyonight.nvim",
    config = function()
      -- require("config.theme").scheme("ayu-dark", "ayu")
    end,
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
    -- module = "treesitter",
    config = function()
      require("config.treesitter")
    end,
  })

  use({ "lukas-reineke/indent-blankline.nvim",
    -- requires = { "treesitter" },
    -- after = { "treesitter" },
    -- TODO Doesn't work if I include config, but does if I don't and run :IndentBlanklineEnable
    config = function()
      require("config.indent-blankline")
    end,
  })

  use({ "akinsho/bufferline.nvim",
    config = function()
      require("config.bufferline")
    end,
  })

  -- use({ "lewis6991/gitsigns.nvim",
  --   config = function()
  --     require("gitsigns").setup({})
  --   end,
  -- })

  use({ "windwp/nvim-autopairs",
    -- after = { "cmp", "treesitter" },
    config = function()
      require('nvim-autopairs').setup({})
    end,
  })

  use({ "nvim-lualine/lualine.nvim",
    config = function()
      require("config.lualine")
    end,
  })

  use({ "anuvyklack/pretty-fold.nvim",
    config = function()
      require("config.pretty-fold")
    end
  })

  use({ "stevearc/dressing.nvim",
    -- module = "dressing",
    event = "BufReadPre",
  })

  use({ "rcarriga/nvim-notify",
    event = "VimEnter",
    -- after = { "dressing" }
    -- requires = { "dressing" },
    config = function()
      vim.notify = require("notify")
    end,
  })

  if pkg.Bootstrap then
    pkg.update()
  end
end

-- preservim/tagbar

return pkg.setup(plugins)