local ok, whichkey = pcall(require, "which-key")
if not ok then return end

whichkey.setup({
  plugins = {
    presets = {
      operators = false,
      -- TODO Remove when https://github.com/folke/which-key.nvim/pull/275 is merged
      windows = false,
    },
  },
  key_labels = {
    ["<CR>"] = "ENT",
    ["<SPACE>"] = "SPC",
    ["<TAB>"] = "TAB",
    ["<ESC>"] = "ESC",
  },
  window = {
    border = "rounded", -- none, single, double, shadow
    margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
    winblend = 0,
  },
  ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
})

local opts = {
  mode = "n", -- NORMAL mode
  prefix = "<leader>",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
  [".."] = { "<CMD>lua require'telescope'.extensions.notify.notify()<CR>", "Show Notifications" },
  [" "] = { "<CMD>nohlsearch<CR>", "Unhighlight Search" },
  b = {
    { "<CMD>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})<CR>",
    "Buffers" },
    d = { "<CMD>Bdelete!<CR>", "Close Buffer" },
  },
  cfg = { "<CMD>e $MYVIMRC<CR>", "Edit Config" },
  e = { "<CMD>NvimTreeToggle<CR>", "Explorer" },
  f = {
    "<CMD>lua require('telescope.builtin').find_files(require('telescope.themes').get_dropdown{previewer = false})<CR>",
    "Find files",
  },
  F = { "<CMD>Telescope live_grep theme=ivy<CR>", "Find Text" },
  l = {
    { "<CMD>set cursorline!<CR>", "Toggle Cursorline" },
    name = "LSP",
    a = { "<CMD>lua vim.lsp.buf.code_action()<CR>", "Code Action" },
    d = {
      "<CMD>Telescope lsp_document_diagnostics<CR>",
      "Document Diagnostics",
    },
    f = { "<CMD>lua vim.lsp.buf.formatting()<CR>", "Format" },
    i = { "<CMD>LspInfo<CR>", "Info" },
    I = { "<CMD>LspInstallInfo<CR>", "Installer Info" },
    j = {
      "<CMD>lua vim.lsp.diagnostic.goto_next()<CR>",
      "Next Diagnostic",
    },
    k = {
      "<CMD>lua vim.lsp.diagnostic.goto_prev()<CR>",
      "Prev Diagnostic",
    },
    -- l = { "<CMD>lua vim.lsp.codelens.run()<CR>", "CodeLens Action" },
    l = { "<CMD>set cursorcolumn!<CR>", "Toggle Cusorcolumn" },
    q = { "<CMD>lua vim.lsp.diagnostic.set_loclist()<CR>", "Quickfix" },
    r = { "<CMD>lua vim.lsp.buf.rename()<CR>", "Rename" },
    s = { "<CMD>Telescope lsp_document_symbols<CR>", "Document Symbols" },
    S = {
      "<CMD>Telescope lsp_dynamic_workspace_symbols<CR>",
      "Workspace Symbols",
    },
    w = {
      "<CMD>Telescope lsp_workspace_diagnostics<CR>",
      "Workspace Diagnostics",
    },
  },
  m = { "<CMD>lua require'util'.ToggleMouse()<CR>", "Toggle Mouse" },
  n = { "<CMD>lua require'util'.CopyMode()<CR>", "Toggle Numbers" },
  p = {
    name = "Packer",
    c = { "<CMD>PackerCompile<CR>", "Compile" },
    i = { "<CMD>PackerInstall<CR>", "Install" },
    s = { "<CMD>PackerSync<CR>", "Sync" },
    S = { "<CMD>PackerStatus<CR>", "Status" },
    u = { "<CMD>PackerUpdate<CR>", "Update" },
  },
  P = { "<CMD>lua require('telescope').extensions.projects.projects()<CR>", "Projects" },
  -- ["<space>"] = { "<CMD>nohlsearch<CR>", "No Highlight" },
  q = { "<CMD>q!<CR>", "Quit" },
  s = {
    name = "Search",
    c = { "<CMD>Telescope colorscheme<CR>", "Colorscheme" },
    h = { "<CMD>Telescope help_tags<CR>", "Find Help" },
    M = { "<CMD>Telescope man_pages<CR>", "Man Pages" },
    r = { "<CMD>Telescope oldfiles<CR>", "Open Recent File" },
    R = { "<CMD>Telescope registers<CR>", "Registers" },
    k = { "<CMD>Telescope keymaps<CR>", "Keymaps" },
    C = { "<CMD>Telescope commands<CR>", "Commands" },
  },
  t = {
    { "<CMD>TroubleToggle<CR>", "Toggle Trouble" },
    name = "Terminal",
    f = { "<CMD>ToggleTerm direction=float<CR>", "Float" },
    h = { "<CMD>ToggleTerm size=10 direction=horizontal<CR>", "Horizontal" },
    v = { "<CMD>ToggleTerm size=80 direction=vertical<CR>", "Vertical" },
  },
  u = { "<CMD>UndotreeToggle<CR><CMD>UndotreeFocus<CR>", "Toggle Undo Tree" },
  w = {
    { "<CMD>w!<CR>", "Write" },
    q = { "<CMD>wq!<CR>", "Write and Quit" },
  },
}

whichkey.register(mappings, opts)
