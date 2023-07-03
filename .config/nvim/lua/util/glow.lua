-- TODO
local ok, toggleterm = pcall(require, "toggleterm.terminal")
if not ok then return end
local Terminal = toggleterm.Terminal

local M = {}

M.Glow = function()
  local file = vim.fn.expand("%:p") -- current file
  if file:sub(-#'.md') ~= '.md' then
    file = vim.fn.expand("%:p:h") -- current directory
  end
  local glow = Terminal:new({ cmd="PAGER='less -r' glow -s dark -p "..file, hidden=false })
  glow:toggle()
end

-- vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua Glow()<CR>", {noremap = true, silent = true})
-- cfg.map("n", "<leader>g", "<CMD>lua Glow()<CR>")

return M