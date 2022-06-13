local globals = { -- {{{
  loaded_gzip = 1,
  loaded_fzf = 1,
  loaded_tar = 1,
  loaded_tarPlugin = 1,
  loaded_zipPlugin = 1,
  loaded_2html_plugin = 1,
  loaded_netrw = 1,
  loaded_netrwPlugin = 1,
  loaded_matchit = 1,
  loaded_matchparen = 1,
}

for k, v in pairs(globals) do
  vim.g[k] = v
end
-- }}}

local options = { -- {{{
  backup = false,                          -- creates a backup file
  clipboard = "unnamedplus",               -- allows neovim to access the system clipboard
  cmdheight = 1,                           -- more space in the neovim command line for displaying messages
  completeopt = { "menuone", "noselect" }, -- mostly just for cmp
  conceallevel = 0,                        -- so that `` is visible in markdown files
  concealcursor = "n",                     -- Hide * markup for bold and italic
  fileencoding = "utf-8",                  -- the encoding written to a file
  hlsearch = true,                         -- highlight all matches on previous search pattern
  ignorecase = true,                       -- ignore case in search patterns
  inccommand = "split",                    -- preview incremental substitute
  mouse = "c",                             -- allow the mouse to be used in neovim
  pumblend = 10,                           -- Popup blend
  pumheight = 10,                          -- Maximum number of entries in a popup
  shiftround = true,                       -- Round indent
  showmode = false,                        -- we don't need to see things like                                                                                                        -- INSERT                -- anymore
  showtabline = 2,                         -- always show tabs
  smartcase = true,                        -- smart case
  autoindent = true,
  smarttab = true,
  smartindent = true,                      -- make indenting smarter again
  splitbelow = true,                       -- force all horizontal splits to go below current window
  splitright = true,                       -- force all vertical splits to go to the right of current window
  swapfile = false,                        -- creates a swapfile
  termguicolors = true,                    -- set term gui colors (most terminals support this)
  undofile = true,                         -- enable persistent undo
  undolevels = 10000,
  updatetime = 300,                        -- faster completion (4000ms default)
  writebackup = false,                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  expandtab = true,                        -- convert tabs to spaces
  grepprg = "rg --vimgrep",
  grepformat = "%f:%l:%c:%m",
  shiftwidth = 2,                          -- the number of spaces inserted for each indentation
  tabstop = 2,                             -- insert 2 spaces for a tab
  softtabstop = 2,
  cursorline = true,                       -- highlight the current line
  cursorcolumn = false,
  number = true,                           -- set numbered lines
  relativenumber = true,                   -- set relative numbered lines
  numberwidth = 4,                         -- set number column width
  signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                            -- display lines as one long line
  linebreak = true,
  scrolloff = 7,
  history = 10000,
  sidescrolloff = 7,
  autoread = false,
  modeline = true,
  modelines = 5,
  errorbells = false,
  visualbell = false,
  synmaxcol = 1000,
  --scrolljump = 0,
  showcmd = false,                         -- NOTE good for seeing leader-maps
  autochdir = false,
  wildmenu = true,
  sessionoptions = { "buffers", "curdir", "tabpages", "winsize" },
  hidden = true,                           -- Enable modified buffers in background, required for toggleterm
  wildmode = "longest:full,full",          -- Command-line completion mode
  backspace = "indent,eol,start",
  lazyredraw = false,
  confirm = true,                          -- confirm to save changes before exiting modified buffer
  magic = true,
  incsearch = true,
  laststatus = 2,
  --mat = 2,
  joinspaces = false,                      -- No double spaces with join after a dot
  -- list = true,                             -- Show some invisible characters (tabs...
  -- listchars = "trail:·,nbsp:⎵,tab:┊» ",    -- ¦┆┊ eol:⏎ (          )
  foldcolumn = "0",
  foldopen = "block,hor,insert,jump,mark,percent,quickfix,search,tag,undo", -- https://stackoverflow.com/q/7034215
  foldmethod = "marker",
  foldmarker = "{{{,}}}",
  -- autowrite = true                      -- enable auto write
  shortmess = vim.opt.shortmess + "cI",
  iskeyword = vim.opt.iskeyword + "-",     -- Treat dash-splitted words as one word
  guifont = "monospace:h17",               -- the font used in graphical neovim applications
}

for k, v in pairs(options) do
  vim.opt[k] = v
end
-- }}}
