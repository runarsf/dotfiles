local cfg = require("util")
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
    -- "projekt0n/github-nvim-theme",
    config = function()
      require("config.theme").scheme({"ayu-dark", "ayu"})
      -- require("config.theme").scheme({"tokyonight", source_conf="pre"})
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
    -- requires = "nvim-treesitter/playground"
    config = function()
      require("config.treesitter")
    end,
  })

  use({ "p00f/nvim-ts-rainbow",
    requires = "nvim-treesitter/nvim-treesitter",
    after = "nvim-treesitter",
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
    requires = {
      "kyazdani42/nvim-web-devicons",
      "moll/vim-bbye",
    },
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
    end,
    requires = {
      "anuvyklack/nvim-keymap-amend",
    },
  })

  use({ "stevearc/dressing.nvim",
    -- module = "dressing",
    event = "BufReadPre",
  })

  use({ "rcarriga/nvim-notify",
    event = "VimEnter",
    -- Also has keybind and telescope integration
    -- after = { "dressing" }
    -- requires = { "dressing" },
    config = function()
      Notify = require("notify")
      vim.notify = Notify
      Notify.setup({
        on_open = function(win)
          vim.api.nvim_win_set_option(win, "wrap", true)
        end,
        render = "minimal",
        states = "fade_in_slide_out",
        max_width = math.floor(vim.o.columns * 0.6), -- Only allow notify to occupy 60% of terminal real-estate
      })
    end,
  })

  use({ "norcalli/nvim-terminal.lua",
    ft = {
      "terminal",
      "toggleterm"
    },
    config = function()
      require("terminal").setup()
    end,
  })

  use({ "akinsho/toggleterm.nvim",
    -- keys = "<M-n>",
    -- cmd = { "ToggleTerm" },
    config = function()
      require("config.toggleterm")
    end,
  })

  -- use({ "karb94/neoscroll.nvim",
  --   config = function()
  --     require("neoscroll").setup({})
  --   end,
  -- })

  use({ "norcalli/nvim-colorizer.lua",
    config = function()
      require("config.nvim-colorizer")
    end,
  })

  use({ "nvim-telescope/telescope.nvim",
    -- opt = true,
    config = function()
      require("config.telescope")
    end,
    -- cmd = "Telescope",
    -- module = "telescope",
    -- keys = { "<leader>ff", "<leader>pp" },
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
    --event = "InsertEnter",
    --opt = true,
    config = function()
      require("config.nvim-cmp")
    end,
    requires = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      { "windwp/nvim-autopairs",
        config = function()
          require("config.autopairs")
        end,
      },
      "saadparwaiz1/cmp_luasnip",
      "L3MON4D3/LuaSnip",
      -- { "L3MON4D3/LuaSnip",
      --   config = function()
      --     require("config.luasnip")
      --   end,
      -- },
      -- "rafamadriz/friendly-snippets",
      -- { "tzachar/cmp-tabnine",
      --   run = "./install.sh",
      --   config = function()
      --     require("cmp_tabnine.config"):setup({})
      --   end,
      -- },
    },
  })

  use({ "neovim/nvim-lspconfig",
    -- after = "cmp-nvim-lsp",
    config = function()
      require("config.lsp")
    end,
    requires = {
      "williamboman/nvim-lsp-installer",
      "tamago324/nlsp-settings.nvim",
      "jose-elias-alvarez/null-ls.nvim",
    },
  })

  use({ "folke/todo-comments.nvim",
    config = function()
      require("config.todo-comments")
    end,
  })

  use({ "folke/trouble.nvim",
    config = function()
      require("trouble").setup({})
    end,
    requires = {
      "kyazdani42/nvim-web-devicons",
    },
  })

  use({ "ggandor/lightspeed.nvim",
    config = function()
     require("lightspeed").setup({})
    end,
  })

  use({ "mbbill/undotree",
    cmd = {
      "UndotreeToggle",
      "UndotreeShow",
    },
  })

  use({ "preservim/tagbar",
    cmd = {
      "TagbarToggle",
      "TagbarOpen",
      "TagbarOpenAutoClose"
    },
  })

  use({ "eraserhd/parinfer-rust",
    run = "cargo build --release",
    ft = "yuck",
  })

  -- use({ "tmhedberg/SimpylFold",
  --   ft = "python",
  --   config = function()
  --     require("config.simpylfold")
  --   end,
  -- })

  use({ "andweeb/presence.nvim",
    config = function()
      require("config.presence")
    end,
  })

  use({ "chentoast/marks.nvim",
    config = function()
     require("marks").setup({})
    end,
  })

  use({ "godlygeek/tabular",
    cmd = "Tabularize"
  })
  use({ "junegunn/vim-easy-align",
    config = function()
      require("config.easy-align")
    end,
  })

  use({ "ollykel/v-vim",
    ft = {"verilog", "vlang"},
  })

  -- use({ "phaazon/hop.nvim",
  --   config = function()
  --     require("hop").setup()
  --   end,
  -- })

  if pkg.Bootstrap then
    pkg.update()
  end
end

-- use({ "kazhala/close-buffers.nvim", cmd = "BDelete" })
-- SmiteshP/nvim-gps
-- plasticboy/vim-markdown
-- godlygeek/tabular

-- "nvim-telescope/telescope-project.nvim",
-- "nvim-telescope/telescope-fzy-native.nvim",

return pkg.setup(plugins)
