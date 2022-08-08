local naughty       = require("naughty")
local inspect       = require("modules.inspect.inspect")

local M = {}

-- gears.debug.dump()
-- awesome-client 'return require("gears.debug").dump_return(mouse.screen.selected_tag:clients())'
M.debug = function(text)
  if text then
    if type(text) == "table" then
      text = inspect(text)
    end
    naughty.notify({text=tostring(text)})
  end
end

M.TLen = function(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

M.TAdd = function(T, K)
  T[K] = true
end

M.TPop = function(T, K)
  T[K] = nil
end

-- Check if table has key; either key, or value if key is a number
M.THas = function(T, K)
  if T[K] ~= nil then return true end
  for k, v in pairs(T) do
    if v == K and type(k) == "number" then return true end
  end
  return false
end

M.IndexOf = function(array, value)
  for i, v in ipairs(array) do
    if v == value then
      return i
    end
  end
  return nil
end

return M
