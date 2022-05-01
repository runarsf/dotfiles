local ok, lualine = pcall(require, "lualine")
if not ok then return end

-- https://github.com/AdamWagner/stackline/issues/42

lualine.setup({})
