local Util = {}
Util.funcs = {}

-- :h map-listing
Util.map = function(mode, key, cmd, opts)
  local defaults = { noremap=true, silent=true }
  opts = vim.tbl_deep_extend("force", defaults, opts or {})

  if type(cmd) == "function" then
    table.insert(Util.funcs, cmd)
    if opts.expr then
      cmd = ([[luaeval('require("util").execute(%d)')]]):format(#Util.funcs)
    else
      cmd = ("<cmd>lua require('util').execute(%d)<cr>"):format(#Util.funcs)
    end
  end

  if opts.buffer ~= nil then
    local buffer = opts.buffer
    opts.buffer = nil
    return vim.api.nvim_buf_set_keymap(buffer, mode, key, cmd, opts)
  else
    return vim.api.nvim_set_keymap(mode, key, cmd, opts)
  end
end

--[[ Util.prequire = function(fn, post_call)
  local ok, fun = pcall(require, fn)
  if ok then
    if post_call then
      fun(post_call)()
    end
    return fun
  end
  return nil
end ]]

Util.CopyMode = function()
  local g = vim.g
  local w = vim.wo
  vim.cmd("stopinsert")
  g.def_number         = g.def_number ~= nil and true or g.def_number
  g.def_relativenumber = g.def_relativenumber ~= nil and true or g.def_relativenumber
  g.def_wrap           = g.def_wrap ~= nil and false or g.def_wrap
  g.def_list           = g.def_list ~= nil and true or g.def_list
  g.def_signcolumn     = g.def_signcolumn ~= nil and "yes" or g.def_signcolumn

  -- vim.wo always returns a number, but expects a bool when assigning
  local int_to_bool = function(int)
    if int == 0 then return false end
    if int == 1 then return true end
    return int
  end

  if w.number or w.relativenumber then
    g.def_number         = int_to_bool(w.number)
    g.def_relativenumber = int_to_bool(w.relativenumber)
    g.def_wrap           = int_to_bool(w.wrap)
    g.def_list           = int_to_bool(w.list)
    g.def_signcolumn     = w.signcolumn

    -- Disable
    w.number         = false
    w.relativenumber = false
    w.wrap           = false
    w.list           = false
    w.signcolumn     = "no"
    vim.cmd("mkview 3")
    vim.api.nvim_input("zRzz")
  else
    -- Restore previous state
    w.number         = g.def_number
    w.relativenumber = g.def_relativenumber
    w.wrap           = g.def_wrap
    w.list           = g.def_list
    w.signcolumn     = g.def_signcolumn
    vim.cmd("loadview 3")
  end
end

return Util