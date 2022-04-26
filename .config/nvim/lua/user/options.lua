local options = {
  backup = false,                          -- creates a backup file
  clipboard = "unnamedplus",               -- allows neovim to access the system clipboard
  cmdheight = 2,                           -- more space in the neovim command line for displaying messages
  completeopt = { "menuone", "noselect" }, -- mostly just for cmp
  conceallevel = 0,                        -- so that `` is visible in markdown files
  fileencoding = "utf-8",                  -- the encoding written to a file
  hlsearch = true,                         -- highlight all matches on previous search pattern
  ignorecase = true,                       -- ignore case in search patterns
  mouse = "c",                             -- allow the mouse to be used in neovim
  pumheight = 10,                          -- pop up menu height
  showmode = false,                        -- we don't need to see things like -- INSERT -- anymore
  showtabline = 2,                         -- always show tabs
  smartcase = true,                        -- smart case
  autoindent = true,
  smarttab = true,
  smartindent = true,                      -- make indenting smarter again
  splitbelow = true,                       -- force all horizontal splits to go below current window
  splitright = true,                       -- force all vertical splits to go to the right of current window
  swapfile = false,                        -- creates a swapfile
  termguicolors = true,                    -- set term gui colors (most terminals support this)
  timeoutlen = 300,                        -- time to wait for a mapped sequence to complete (in milliseconds)
  ttimeoutlen = 50,                        -- affects escape from insert mode
  undofile = true,                         -- enable persistent undo
  updatetime = 300,                        -- faster completion (4000ms default)
  writebackup = false,                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  expandtab = true,                        -- convert tabs to spaces
  shiftwidth = 2,                          -- the number of spaces inserted for each indentation
  tabstop = 2,                             -- insert 2 spaces for a tab
  softtabstop = 0,
  cursorline = true,                       -- highlight the current line
  cursorcolumn = false,
  number = true,                           -- set numbered lines
  relativenumber = true,                   -- set relative numbered lines
  numberwidth = 4,                         -- set number column width
  signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                            -- display lines as one long line
  scrolloff = 7,
  history = 10000,
  sidescrolloff = 7,
  autoread = false,
  cmdheight = 1,
  modeline = true,
  modelines = 5,
  errorbells = false,
  visualbell = false,
  synmaxcol = 1000,
  -- scrolljump = 0,
  showcmd = false,
  autochdir = false,
  wildmenu = true,
  hidden = false,
  wildmode = "longest:full,full",
  backspace = "indent,eol,start",
  lazyredraw = false,
  magic = true,
  incsearch = true,
  laststatus = 2,
  -- mat = 2,
  list = true,
  listchars = "trail:·,nbsp:⎵,tab:┊» ",    -- ¦┆┊ eol:⏎ (          )
  foldmethod = "marker",
  foldmarker = "{{{,}}}",
  guifont = "monospace:h17",               -- the font used in graphical neovim applications
}

vim.opt.shortmess:append "cI"

for k, v in pairs(options) do
  vim.opt[k] = v
end

-- Automatically go to next line - https://vim.fandom.com/wiki/Automatically_wrap_left_and_right
-- vim.cmd "set whichwrap+=<,>,[,],h,l"
-- vim.cmd "set whichwrap+=<,>,h,l"
-- Treat dash-splitted words as one word
vim.cmd [[set iskeyword+=-]]
-- vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work
-- vim.cmd [[set path+=**]]
-- vim.cmd [[set wildignore+=**/node_modules/**]]
