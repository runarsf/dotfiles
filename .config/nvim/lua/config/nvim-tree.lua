local ok, nvimtree = pcall(require, "nvim-tree")
if not ok then return end

-- local ok, nvimtree_conf = pcall(require, "nvim-tree.config")
-- if not ok then return end

-- local tree_cb = nvimtree_conf.nvim_tree_callback

nvimtree.setup({
  disable_netrw = true,
  update_focused_file = {
    enable = true,
    update_cwd = true,
  },
  update_cwd = true,
  filters = {
    custom = { ".git", "node_modules", ".cargo" },
  },
  actions = {
    open_file = {
      quit_on_open = true,
      window_picker = {
        enable = false,
      },
    },
  },
  view = {
    width = 25,
    -- height = 30,
    hide_root_folder = false,
    side = "left",
    -- auto_resize = true,
    mappings = {
      custom_only = false,
    },
    number = false,
    relativenumber = false,
  },
})

-- These need to be mapped somewhere else if nvim-tree is lazy-loaded
-- cfg.map("n", "<C-n>", ":NvimTreeToggle<CR>")
-- cfg.map("v", "<C-n>", "<ESC>:NvimTreeToggle<CR>")
-- cfg.map("i", "<C-n>", "<ESC>:NvimTreeToggle<CR>")

-- https://github.com/nvim-tree/nvim-tree.lua/wiki/Open-At-Startup
local function open_nvim_tree(data)
  local ignored_ft = {
    "startify",
    "dashboard",
    "alpha",
  }

  local real_file = vim.fn.filereadable(data.file) == 1
  local no_name = data.file == "" and vim.bo[data.buf].buftype == ""
  local directory = vim.fn.isdirectory(data.file) == 1
  local filetype = vim.bo[data.buf].ft

  if vim.tbl_contains(ignored_ft, filetype) then
    return
  end

  if no_name and not real_file then
    require("nvim-tree.api").tree.toggle({ focus = false, find_file = true, })
  elseif directory then
    -- create a new, empty buffer
    vim.cmd.enew()
    -- wipe the directory buffer
    vim.cmd.bw(data.buf)

    -- change to the directory
    vim.cmd.cd(data.file)
    require("nvim-tree.api").tree.open()
  end
end

vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = open_nvim_tree })


-- https://github.com/nvim-tree/nvim-tree.lua/wiki/Auto-Close
local function tab_win_closed(winnr)
  local api = require"nvim-tree.api"
  local tabnr = vim.api.nvim_win_get_tabpage(winnr)
  local bufnr = vim.api.nvim_win_get_buf(winnr)
  local buf_info = vim.fn.getbufinfo(bufnr)[1]
  local tab_wins = vim.tbl_filter(function(w) return w~=winnr end, vim.api.nvim_tabpage_list_wins(tabnr))
  local tab_bufs = vim.tbl_map(vim.api.nvim_win_get_buf, tab_wins)
  if buf_info.name:match(".*NvimTree_%d*$") then            -- close buffer was nvim tree
    -- Close all nvim tree on :q
    if not vim.tbl_isempty(tab_bufs) then                      -- and was not the last window (not closed automatically by code below)
      api.tree.close()
    end
  else                                                      -- else closed buffer was normal buffer
    if #tab_bufs == 1 then                                    -- if there is only 1 buffer left in the tab
      local last_buf_info = vim.fn.getbufinfo(tab_bufs[1])[1]
      if last_buf_info.name:match(".*NvimTree_%d*$") then       -- and that buffer is nvim tree
        vim.schedule(function ()
          if #vim.api.nvim_list_wins() == 1 then                -- if its the last buffer in vim
            vim.cmd "quit"                                        -- then close all of vim
          else                                                  -- else there are more tabs open
            vim.api.nvim_win_close(tab_wins[1], true)             -- then close only the tab
          end
        end)
      end
    end
  end
end

vim.api.nvim_create_autocmd("WinClosed", {
  callback = function ()
    local winnr = tonumber(vim.fn.expand("<amatch>"))
    vim.schedule_wrap(tab_win_closed(winnr))
  end,
  nested = true
})
