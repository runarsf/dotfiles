local shape = require("gears.shape")
local awful     = require("awful")
local naughty = require("naughty")
local inspect = require("modules.inspect.inspect")
local beautiful = require("beautiful")

-- Enable sloppy focus, so that focus follows mouse {{{
client.connect_signal("mouse::enter", function(c)
  c:activate { context="mouse_enter", raise=false }
end)
-- }}}

-- Spawn new clients as slave and make sure they don't leave the boundaries {{{
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)
-- }}}

-- Spawn client under active client {{{
-- client.connect_signal("manage", function(c)
--   awful.client.setslave(c)
--   local prev_focused = awful.client.focus.history.get(awful.screen.focused(), 1, nil)
--   local prev_c = awful.client.next(-1, c)
--   if prev_c and prev_focused then
--     while prev_c ~= prev_focused do
--       c:swap(prev_c)
--       prev_c = awful.client.next(-1, c)
--     end
--   end
-- 
--   if awesome.startup and
--     not c.size_hints.user_position and
--     not c.size_hints.program_position then
--       -- Prevent clients from being unreachable after screen count changes.
--       awful.placement.no_offscreen(c)
--   end
-- end)
-- }}}

--[[ No borders when only one client {{{
-- https://stackoverflow.com/a/51687321
screen.connect_signal("arrange", function(s)
  local max = s.selected_tag.layout.name == "max"
  local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
  -- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
  for _, c in pairs(s.clients) do
    if (max or only_one) and not c.floating or c.maximized then
      c.border_width = 0
    else
      c.border_width = beautiful.border_width
    end
  end
end)
-- }}} ]]

-- Fullscreen nodes on top {{{
-- client.connect_signal("property::fullscreen", function(c)
--   if c.fullscreen then
--     c.above = false
--   else
--     c.above = true
--   end
-- end)
-- }}}

-- TODO helpers.lua
local THas = function(T, K)
  if T[K] ~= nil then return true end
  for k, v in pairs(T) do
    if v == K and type(k) == "number" then return true end
  end
  return false
end

-- Center floating nodes and give them a titlebar {{{
local floating_handler = function(c)
  if not (c.maximized or c.fullscreen) then
    -- FIXME `attempt to index a nil value (field 'selected_tag')` when attempt to focus tag when scratchpad closed
    if (c.floating or c.screen.selected_tag.layout.name == "floating") and THas({"normal", "dialog"}, c.type) then
      if not c.requests_no_titlebar then
        awful.titlebar.show(c)
      else
        awful.titlebar.hide(c)
      end
      c.above = true
      -- c.shape = function(cr,w,h)
      --   shape.rounded_rect(cr,w,h,5)
      -- end
      return true
    else
      awful.titlebar.hide(c)
      c.above = false
      -- c.shape = shape.rectangle
    end
  end
  return false
end

-- client.connect_signal("request::border", function(c)
--   if floating_handler(c) then
--     awful.placement.centered(c)
--   end
-- end)
client.connect_signal("manage", floating_handler)
client.connect_signal("property::floating", function(c)
  if floating_handler(c) then
    awful.placement.centered(c)
  end
end)
tag.connect_signal("property::layout", function(t)
  local clients = t:clients()
  for k,c in pairs(clients) do
    floating_handler(c)
  end
end)
-- }}}

-- Disable fullscreen on focus lost {{{
client.connect_signal("mouse::leave", function(c)
  if c.fullscreen then
    c.fullscreen = false
  end
end)
-- }}}

-- Rounded borders without a compositor {{{
local shape_handler = function(c)
  if c.fullscreen then
    c.shape = shape.rectangle
  else
    c.shape = function(cr,w,h)
      shape.rounded_rect(cr,w,h,5)
    end
    -- c.shape = function(cr, w, h)
    --   -- shape.transform(shape.rectangle) : translate(0,     0)(cr, w/2, h/2)
    --   -- shape.transform(shape.rectangle) : translate(0,   w/2)(cr, w/2, h/2)
    --   -- shape.transform(shape.rectangle) : translate(h/2,   0)(cr, w/2, h/2)
    --   -- shape.transform(shape.rectangle) : translate(h/2, w/2)(cr, w/2, h/2)

    --   -- shape.transform(shape.circle)    : translate(0,     0)(cr, w/2, h/2)
    --   -- shape.transform(shape.circle)    : translate(0,   h/2)(cr, w/2, h/2)
    --   -- shape.transform(shape.partially_rounded_rect) : translate(w/2, h/5)(cr, w/2, h/2, false, true, true, false, 100)
    -- end
  end
end
-- client.connect_signal("request::border", shape_handler)
-- client.connect_signal("manage", shape_handler)
-- client.connect_signal("property::fullscreen", shape_handler)
-- }}}
