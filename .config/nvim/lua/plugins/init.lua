return {
  { "Shatur/neovim-ayu",
    config = function()
      vim.cmd("highlight Folded guibg=#1E2030")
    end,
  },

  { "nvim-lua/popup.nvim" },            -- Popup API from Vim in Neovim
  { "nvim-lua/plenary.nvim" },            -- Useful lua functions
  { "antoinemadec/FixCursorHold.nvim" },  -- Fixes lsp doc highlight
  { "lewis6991/impatient.nvim" },         -- Speed up Lua module loading
  { "nathom/filetype.nvim" },             -- Speed up startup time

  { "LazyVim/LazyVim",
    opts = {
      colorscheme = "ayu-dark",
    },
  },

  -- { "andweeb/presence.nvim", },

  { "simrat39/symbols-outline.nvim",
    cmd = "SymbolsOutline",
    keys = { { "<leader>cs", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" } },
    config = true,
  },

  { "folke/which-key.nvim",
    opts = {
      defaults = {
        ["<leader>"] = {
          [".."] = { "<CMD>lua require'telescope'.extensions.notify.notify()<CR>", "Show Notifications" },
          ["<SPACE>"] = { "<CMD>nohlsearch<CR>", "Unhighlight Search" },
          L = {
            { "<CMD>set cursorline!<CR>", "Toggle Cursorline" },
            name = "LSP",
            L = { "<CMD>set cursorcolumn!<CR>", "Toggle Cusorcolumn" },
          },
          m = {
            { "<CMD>lua require'util'.ToggleMouse()<CR>", "Toggle Mouse" },
            d = { "<CMD>w<CR><CMD>MarkdownPreview<CR>", "Markdown Preview (web)" },
            g = { "<CMD>w<CR><CMD>lua require'config.toggleterm'.Glow()<CR>", "Markdown Preview (glow)" },
          },
          n = { "<CMD>lua require'util'.CopyMode()<CR>", "Toggle Numbers" },
          P = {
            "<CMD>Legendary<CR>",
            "Command Palette",
          },
          q = { "<CMD>q<CR>", "Quit" },
          r = {
            name = "REST Client",
            r = { "<Plug>RestNvim", "Run Under Cursor" },
            c = { "<Plug>RestNvimPreview", "Preview CURL Command" },
            a = { "<Plug>RestNvimLast", "Re-Run Last Request" },
          },
          T = {
            "<CMD>TodoTelescope<CR>",
            "TODO"
          },
          u = { "<CMD>UndotreeToggle<CR><CMD>UndotreeFocus<CR>", "Toggle Undo Tree" },
          w = {
            { "<CMD>w!<CR>", "Write" },
            q = { "<CMD>wq!<CR>", "Write and Quit" },
            r = { "<CMD>set wrap!<CR>", "Toggle Word Wrap" },
          },
        }
      }
    }
  },

  { "hrsh7th/nvim-cmp",
    dependencies = { "hrsh7th/cmp-emoji" },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, { { name = "emoji" } }))
    end,
  },

  { "telescope.nvim",
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-lua/plenary.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
  },

  -- https://www.lazyvim.org/plugins/ui#alpha-nvim
  { "alpha-nvim",
    opts = function()
      local alpha = require("alpha")
      local dashboard = require("alpha.themes.dashboard")
      require("alpha.term")
      
      dashboard.section.header.type = "terminal"
      dashboard.section.header.command = "cat ~/.config/dotfiles/ascii.txt"
      dashboard.section.header.width = 28
      dashboard.section.header.height = 14
      dashboard.section.header.opts.position = "center"
      dashboard.section.header.opts.hl = "String"

      dashboard.opts.layout[1].val = 1
  
      return dashboard
    end,
  },

  { "folke/todo-comments.nvim",
    opts = {
      highlight = {
        pattern = [[.*<(KEYWORDS)\s*:*]],
      },
      search = {
        pattern = [[\s\b(KEYWORDS)\b\s]]
      }
    },
  },

  { "mbbill/undotree",
    cmd = {
      "UndotreeToggle",
      "UndotreeShow",
    },
  },

  { "phaazon/hop.nvim",
    branch = "v2",
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
    end,
  },

  { "mrjones2014/legendary.nvim",
    config = function()
      require("legendary").setup({
        which_key = {
          auto_register = true,
          select_prompt = "Command Palette",
        }
      })
    end,
    dependencies = { "kkharji/sqlite.lua" },
  },

  { "chentoast/marks.nvim",
    config = function()
      require("marks").setup({})
    end,
  },

  { "rest-nvim/rest.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function ()
      require("rest-nvim").setup({
        skip_ssl_verification = true,
      })
    end,
  },

  --[[ { "norcalli/nvim-colorizer.lua",
    config = function()
      require("config.nvim-colorizer")
    end,
  }, ]]

  { "NoahTheDuke/vim-just",
    ft = { "just", "justfile" },
  },
  { "elkowar/yuck.vim",
    ft = { "yuck", "lisp" },
  },
  { "vim-scripts/paredit.vim",
    ft = { "yuck", "lisp" },
  },
  { "eraserhd/parinfer-rust",
    build = "cargo build --release",
    ft = { "yuck", "lisp" },
  },

  { "neovim/nvim-lspconfig",
    ---@class PluginLspOpts
    opts = {
      ---@type lspconfig.options
      servers = {
        -- pyright will be automatically installed with mason and loaded with lspconfig
        pyright = {},
      },
      autoformat = false,
    },
  },

  { "kaarmu/typst.vim",
    ft = "typst",
  },

  { "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "mrjones2014/nvim-ts-rainbow",
    },
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, {
        "commonlisp",
        "cpp",
        "css",
        "dart",
        "dockerfile",
        "go",
        "graphql",
        "haskell",
        "hcl",
        "http",
        "java",
        "javascript",
        "jsdoc",
        "jsonc",
        "latex",
        "make",
        "nix",
        "ruby",
        "rust",
        "scss",
        "toml",
        "vue",
      })
      rainbow = {
        enable = true,
        extended_mode = true,
        max_file_lines = nil,
      }
    end,
  },
  
  { "iamcco/markdown-preview.nvim",
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },

  { "anuvyklack/fold-preview.nvim",
    dependencies = { "anuvyklack/keymap-amend.nvim" },
    config = function()
      require("fold-preview").setup()
    end,
  },

  { "anuvyklack/pretty-fold.nvim",
    dependencies = { "anuvyklack/nvim-keymap-amend" },
    opts = {
      keep_indentation = false,
      fill_char = ' ',
      sections = {
        left = {
          '»', 'content', function() return string.rep('›', vim.v.foldlevel - 1) end
        },
        right = {
          '{ ', 'number_of_folded_lines', ' }  ',
        }
      }
    },
  },

  { "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "stylua",
        "shellcheck",
        "shfmt",
        "flake8",
      },
    },
  },

  -- Use <tab> for completion and snippets (supertab)
  -- first: disable default <tab> and <s-tab> behavior in LuaSnip
  { "L3MON4D3/LuaSnip",
    keys = function()
      return {}
    end,
  },
  -- then: setup supertab in cmp
  { "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-emoji",
    },
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      local luasnip = require("luasnip")
      local cmp = require("cmp")

      opts.mapping = vim.tbl_extend("force", opts.mapping, {
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
            -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
            -- this way you will only jump inside the snippet region
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
    end,
  },

  { "akinsho/toggleterm.nvim",
    version = "*",
    keys = {
      { "<M-n>",
        function()
          vim.cmd("ToggleTerm")
        end,
        desc = "Toggle terminal"
      }
    },
    config = function()
      local cfg = require("util")

      local ok, toggleterm = pcall(require, "toggleterm")
      if not ok then return end

      vim.opt.hidden = true

      cfg.map("t", "<ESC><ESC>", "<C-\\><C-N>")

      toggleterm.setup({
        open_mapping = "<M-n>",
        size = 20,
        shading_factor = 2,
        direction = "float",
        float_opts = {
          border = "curved",
          winblend = 0,
          highlights = {
            border = "Normal",
            background = "Normal",
          },
        },
      })
    end,
    dependencies = {
      { "norcalli/nvim-terminal.lua",
        ft = {
          "terminal",
          "toggleterm"
        },
        config = function()
          require("terminal").setup()
        end,
      }
    },
  },
}
