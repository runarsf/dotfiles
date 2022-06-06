local awful     = require("awful")
local beautiful = require("beautiful")

-- Enable sloppy focus, so that focus follows mouse {{{
client.connect_signal("mouse::enter", function(c)
  c:activate { context="mouse_enter", raise=false }
end)
-- }}}

-- Spawn client under active client {{{
client.connect_signal("manage", function(c)
  if not awesome.startup then
    awful.client.setslave(c)
    local prev_focused = awful.client.focus.history.get(awful.screen.focused(), 1, nil)
    local prev_c = awful.client.next(-1, c)
    if prev_c and prev_focused then
      while prev_c ~= prev_focused do
        c:swap(prev_c)
        prev_c = awful.client.next(-1, c)
      end
    end
  end

  if awesome.startup and
    not c.size_hints.user_position and
    not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
  end
end)
-- }}}

-- No borders when only one client {{{
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
-- }}}

-- Center floating nodes and give them a titlebar {{{
local floating_handler = function(c)
  if c.floating or
     c.screen.selected_tag.layout.name == "floating" then
    awful.titlebar.show(c)
    return true
  else
    awful.titlebar.hide(c)
    return false
  end
end

client.connect_signal("property::floating", function(c)
  if floating_handler(c) then
    awful.placement.centered(c)
  end
end)
client.connect_signal("manage", function(c)
  floating_handler(c)
end)
tag.connect_signal("property::layout", function(t)
  local clients = t:clients()
  for k,c in pairs(clients) do
    floating_handler(c)
  end
end)
-- }}}
