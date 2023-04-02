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

  use({ "folke/which-key.nvim",
    --event = "VimEnter",
    config = function()
      require("config.which-key")
    end,
  })

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
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require("config.nvim-tree")
    end,
  })

  use({ "lmburns/lf.nvim",
    config = function()
      vim.g.lf_netrw = 0

      require("lf").setup({
        escape_quit = false,
        border = "rounded",
        highlights = {FloatBorder = {guifg = require("kimbox.palette").colors.magenta}}
      })
    end,
    requires = {"plenary.nvim", "toggleterm.nvim", "kimbox"}
  })
  use({ "lmburns/kimbox",
    config = function()
      require("kimbox").setup({})
    end,
  })

  use({ "bennypowers/nvim-regexplainer",
    config = function()
      require("regexplainer").setup()
    end,
    requires = {
      "nvim-treesitter/nvim-treesitter",
      "MunifTanjim/nui.nvim",
    },
  })

  -- use({ "SeniorMars/typst.nvim" })

  -- use({ "xiyaowong/virtcolumn.nvim" })

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

  -- use({ "p00f/nvim-ts-rainbow",
  use({ "mrjones2014/nvim-ts-rainbow",
    requires = "nvim-treesitter/nvim-treesitter",
    after = "nvim-treesitter",
  })

  use({ "lukas-reineke/indent-blankline.nvim",
    -- requires = { "treesitter" },
    -- after = { "treesitter" },
    -- TODO Doesn't work if I include config, but does if I don"t and run :IndentBlanklineEnable
    config = function()
      require("config.indent-blankline")
    end,
  })

  use({ "iamcco/markdown-preview.nvim",
    run = function()
      vim.fn["mkdp#util#install"]()
    end,
  })

  -- use({ "akinsho/bufferline.nvim",
  --   config = function()
  --     require("config.bufferline")
  --   end,
  --   requires = {
  --     "kyazdani42/nvim-web-devicons",
  --     "moll/vim-bbye",
  --   },
  -- })

  use({ "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup({})
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
    end,
    requires = {
      "anuvyklack/nvim-keymap-amend",
    },
  })
  use({ "anuvyklack/fold-preview.nvim",
    requires = "anuvyklack/keymap-amend.nvim",
    config = function()
      require("fold-preview").setup()
    end
  })

  use({ "stevearc/dressing.nvim",
    -- module = "dressing",
    event = "BufReadPre",
  })

  -- NOTE https://github.com/folke/noice.nvim/issues/321
  use({ "folke/noice.nvim",
    config = function()
      require("noice").setup({
        lsp = {
          override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
          },
        },
        presets = {
          bottom_search = true, -- use a classic bottom cmdline for search
          command_palette = true, -- position the cmdline and popupmenu together
          long_message_to_split = true, -- long messages will be sent to a split
          inc_rename = false, -- enables an input dialog for inc-rename.nvim
          lsp_doc_border = false, -- add a border to hover docs and signature help
        },
      })
    end,
    requires = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    }
  })

  use({ "rcarriga/nvim-notify",
    event = "VimEnter",
    -- Also has keybind and telescope integration
    requires = "stevearc/dressing.nvim",
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

  use({ "norcalli/nvim-colorizer.lua",
    config = function()
      require("config.nvim-colorizer")
    end,
  })

  -- use({ "echasnovski/mini.nvim",
  --   config = function()
  --     require("mini.starter").setup()
  --   end,
  -- })

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
      "crispgm/telescope-heading.nvim"
    },
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
      --  "jose-elias-alvarez/null-ls.nvim",
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

  use({ "mbbill/undotree",
    cmd = {
      "UndotreeToggle",
      "UndotreeShow",
    },
  })

  use({ "rest-nvim/rest.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function ()
      require("rest-nvim").setup({
        skip_ssl_verification = true,
      })
     end,
  })

  use({ "kwkarlwang/bufjump.nvim",
    config = function()
      require("bufjump").setup({
        forward = "<C-l>",
        backward = "<C-h>",
      })
    end,
  })

  use ({ "phaazon/hop.nvim",
    branch = "v2", -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require"hop".setup { keys = "etovxqpdygfblzhckisuran" }
      local hop = require("hop")
      local directions = require("hop.hint").HintDirection
      vim.keymap.set("", "f", function()
        hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
      end, {remap=true})
      vim.keymap.set("", "F", function()
        hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
      end, {remap=true})
      vim.keymap.set("", "t", function()
        hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
      end, {remap=true})
      vim.keymap.set("", "T", function()
        hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
      end, {remap=true})
      vim.keymap.set("", "s", "<CMD>HopPattern<CR>")
      vim.keymap.set("", "S", "<CMD>HopAnywhere<CR>")
    end
  })

  -- use({ "preservim/tagbar",
  --   cmd = {
  --     "TagbarToggle",
  --     "TagbarOpen",
  --     "TagbarOpenAutoClose"
  --   },
  -- })

  use({ "chentoast/marks.nvim",
    config = function()
     require("marks").setup({})
    end,
  })

  use({ "mrjones2014/legendary.nvim",
    config = function()
      require("legendary").setup({
        which_key = {
          auto_register = true,
          select_prompt = "Command Palette",
        }
      })
    end,
    requires = "kkharji/sqlite.lua",
  })

  use({ "elkowar/yuck.vim",
    ft = {"yuck", "lisp"},
  })
  use({ "vim-scripts/paredit.vim",
    ft = {"yuck", "lisp"},
  })
  use({ "eraserhd/parinfer-rust",
    run = "cargo build --release",
    ft = {"yuck", "lisp"},
  })

  use({ "NMAC427/guess-indent.nvim",
    config = function()
      require("guess-indent").setup({})
    end,
  })

  -- use({ "mg979/vim-visual-multi",
  --   config = function()
  --     vim.g["VM_default_mappings"] = 0
  --     vim.g["VM_maps"] = {}
  --   end,
  -- })

  if pkg.Bootstrap then
    pkg.update()
  end
end

return pkg.setup(plugins)
