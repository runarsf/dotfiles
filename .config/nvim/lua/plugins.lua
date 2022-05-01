local pkg = require("util.pkg")

local plugins = function(use)
  use({ "wbthomason/packer.nvim", opt=true }) -- Have packer manage itself

  -- Utility --
  use({ "nvim-lua/popup.nvim" })              -- Popup API from Vim in Neovim
  use({ "nvim-lua/plenary.nvim" })            -- Useful lua functions
  use({ "antoinemadec/FixCursorHold.nvim" })  -- Fixes lsp doc highlight
  use({ "lewis6991/impatient.nvim" })         -- Speed up Lua module loading
  use({ "nathom/filetype.nvim" })             -- Speed up startup time

  use({ -- Themes
    "Shatur/neovim-ayu",
    -- "folke/tokyonight.nvim",
    config = function()
      require("config.theme").scheme("ayu-dark", "ayu")
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

  use({ "norcalli/nvim-terminal.lua",
    ft = "terminal",
    config = function()
      require("terminal").setup()
    end,
  })

  use({ "akinsho/toggleterm.nvim",
    keys = "<M-n>",
    cmd = { "ToggleTerm" },
    config = function()
      require("config.toggleterm")
    end,
  })

  use({ "karb94/neoscroll.nvim",
    config = function()
      require("neoscroll").setup({})
    end,
  })

  use({ "norcalli/nvim-colorizer.lua",
    config = function()
      require("config.nvim-colorizer")
    end,
  })

  use({ "nvim-telescope/telescope.nvim",
    opt = true,
    config = function()
      require("config.telescope")
    end,
    cmd = { "Telescope" },
    module = "telescope",
    keys = { "<leader>ff", "<leader>pp" },
    requires = {
      "nvim-telescope/telescope-z.nvim",
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-symbols.nvim",
    },
  })

  use({ "folke/which-key.nvim",
    event = "VimEnter",
    config = function()
      require("config.which-key")
    end,
  })

  use({ "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    opt = true,
    config = function()
      require("config.nvim-cmp")
    end,
    requires = {
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lsp",
      "saadparwaiz1/cmp_luasnip",
      "rafamadriz/friendly-snippets",
      { "L3MON4D3/LuaSnip",
        config = function()
          require("config.luasnip")
        end,
      },
      { "windwp/nvim-autopairs",
        module = "nvim-autopairs",
        config = function()
          require('nvim-autopairs').setup({})
        end,
      }
    },
  })

  if pkg.Bootstrap then
    pkg.update()
  end
end

-- use({ "kazhala/close-buffers.nvim", cmd = "BDelete" })
-- preservim/tagbar
-- "moll/vim-bbye"
-- SmiteshP/nvim-gps
-- simrat39/symbols-outline.nvim
-- npxbr/glow.nvim
-- plasticboy/vim-markdown
-- godlygeek/tabular
-- mbbill/undotree

-- phaazon/hop.nvim
-- ggandor/lightspeed.nvim

-- folke/trouble.nvim
-- folke/todo-comments.nvim

-- "nvim-telescope/telescope-project.nvim",
-- "nvim-telescope/telescope-fzy-native.nvim",

return pkg.setup(plugins)