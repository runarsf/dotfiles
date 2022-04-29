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

return Util