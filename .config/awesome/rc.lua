--[[{{{ Resources
 - https://awesomewm.org/apidoc/
 - https://github.com/atsepkov/awesome-awesome-wm
 - Convert unicode character to png icon:
   $ convert -background transparent -fill '#E6E6E6' -font ~/.fonts/JetBrains/JetBrains\ Mono\ Bold\ Italic\ Nerd\ Font\ Complete\ Mono.ttf -pointsize 48 -size 64x64 -gravity center label:TEXT output.png
 - Debug lua:
   $ awesome-client 'return require("gears").filesystem.get_configuration_dir()'
 - https://github.com/frioux/charitable
 - https://github.com/Drauthius/awesome-sharedtags
 - https://stackoverflow.com/questions/69574689/how-to-limit-the-width-of-window-entries-on-the-wibar
 - https://github.com/mut-ex/awesome-wm-nice
 - https://www.reddit.com/r/awesomewm/comments/gehk1g/cursor_follows_focus_possible/
 - https://awesomewm.org/recipes/
 - https://www.reddit.com/r/awesomewm/comments/mfklx5/shadow_only_for_floating_windows/
 - https://www.reddit.com/r/awesomewm/comments/bki1md/show_titlebar_only_when_window_is_floating/
 - https://www.reddit.com/r/awesomewm/comments/box4jk/my_functional_dynamic_border_gap_and_titlebar/
--}}}]]

-- {{{ Packages
pcall(require, "luarocks.loader")

-- Dynamic tags
require("eminent.eminent")
-- Scratchpad
local scratch       = require("scratch.scratch")
-- XMonad-like workspaces
local charitable    = require("charitable")
-- OSX-like titlebars
local nice          = require("nice")
local lain          = require("lain")

local gears         = require("gears")
local awful         = require("awful")
require("awful.autofocus")
local wibox         = require("wibox")
local beautiful     = require("beautiful")
local naughty       = require("naughty")
local ruled         = require("ruled")
local menubar       = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")
local xresources    = require("beautiful.xresources")
local dpi           = xresources.apply_dpi
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (this code will only ever execute for the fallback config).
naughty.connect_signal("request::display_error", function(message, startup)
  naughty.notification {
    urgency = "critical",
    title   = "Oops, an error happened"..(startup and " during startup!" or "!"),
    message = message
  }
end)
-- }}}

-- {{{ Variables
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")

-- WARN: nice breaks with gtk4, temporary workaround is to uninstall it - https://github.com/mut-ex/awesome-wm-nice/issues/22

--[[+NICE
nice {
  -- To disable titlebars, set titlebar_height and titlebar_radius to 3,
  -- and remove the titlebar_items or set button_size to 0.
  -- titlebar_color = beautiful.bg
  titlebar_height = 28,
  titlebar_radius = 9,
  titlebar_font = beautiful.font,
  win_shade_enabled = false,
  no_titlebar_maximized = false,
  button_size = 12,
  titlebar_items = {
    -- {"sticky", "ontop", "floating"},
    left = {"close", "minimize", "maximize"},
    middle = "title",
    right = {}
  }
}
--]]

-- awesome.set_preferred_icon_size(32)

-- beautiful.useless_gap = 3
-- beautiful.gap_single_client = true

awful.mouse.snap.edge_enabled = false

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

modkey = "Mod4"
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
  { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
  { "manual", terminal .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({
  items = {
    { "awesome", myawesomemenu, beautiful.awesome_icon },
    { "open terminal", terminal }
  }
})

mylauncher = awful.widget.launcher({
  image = beautiful.awesome_icon,
  menu = mymainmenu
})

menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Layouts
tag.connect_signal("request::default_layouts", function()
  awful.layout.append_default_layouts({
    awful.layout.suit.tile,
    lain.layout.centerwork,
    awful.layout.suit.max,
    -- awful.layout.suit.floating,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair.horizontal,
    awful.layout.suit.fair,
    awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.corner.nw,
  })
end)
-- }}}

-- {{{ Wallpaper
screen.connect_signal("request::wallpaper", function(s)
  awful.wallpaper {
    screen = s,
    widget = {
      {
        image     = beautiful.wallpaper,
        upscale   = true,
        downscale = true,
        widget    = wibox.widget.imagebox,
      },
      valign = "center",
      halign = "center",
      tiled  = false,
      widget = wibox.container.tile,
    }
  }
end)
-- }}}

--[[{{{ Wibar
https://awesomewm.org/apidoc/popups_and_bars/awful.wibar.html 
--]]

-- [[+CHARITABLE
local tags = charitable.create_tags(
   { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
   {
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
   }
)
--]]

screen.connect_signal("request::desktop_decoration", function(s)
  -- Each screen has its own tag table.
  --[[-CHARITABLE
  -- awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])
  awful.tag({ "  ", "  ", "  ", " ﮑ ", " 龎 ", " ﮠ ", "  ", " 煉 ", "  " }, s, awful.layout.layouts[1])
  --]]

  -- [[+CHARITABLE
  -- Show an unselected tag when a screen is connected
  for i = 1, #tags do
    if not tags[i].selected then
      tags[i].screen = s
      tags[i]:view_only()
      break
    end
  end

  -- create a special scratch tag for double buffering
  s.scratch = awful.tag.add('~' .. s.index, {})

  s.mytaglist = awful.widget.taglist({
    screen = s,
    filter = awful.widget.taglist.filter.all,
    buttons = gears.table.join(
      awful.button({ }, 1, function(t) charitable.select_tag(t, awful.screen.focused()) end),
      awful.button({ }, 3, function(t) charitable.toggle_tag(t, awful.screen.focused()) end)
    ),
    source = function(screen, args) return tags end,
  })
  --]]

  --[[-CHARITABLE
  s.mytaglist = awful.widget.taglist {
    screen  = s,
    filter  = awful.widget.taglist.filter.all,
    buttons = {
      -- TODO view_only -> same function as super+#
      awful.button({ }, 1, function(t) t:view_only() end),
      awful.button({ modkey }, 3, function(t)
                                    if client.focus then
                                      client.focus:move_to_tag(t)
                                    end
                                  end),
      awful.button({ }, 3, awful.tag.viewtoggle),
      awful.button({ modkey }, 1, function(t)
                                    if client.focus then
                                      client.focus:toggle_tag(t)
                                    end
                                  end),
      awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
      awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
    }
  }
  --]]

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox {
    screen  = s,
    buttons = {
      awful.button({ }, 1, function() awful.layout.inc( 1) end),
      awful.button({ }, 3, function() awful.layout.inc(-1) end),
      awful.button({ }, 4, function() awful.layout.inc(-1) end),
      awful.button({ }, 5, function() awful.layout.inc( 1) end),
    }
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = {
      awful.button({ }, 1, function(c) c:activate { context="tasklist", action="toggle_minimization" } end),
      awful.button({ }, 3, function() awful.menu.client_list { theme={ width=250 } } end),
      awful.button({ }, 4, function() awful.client.focus.byidx(-1) end),
      awful.button({ }, 5, function() awful.client.focus.byidx(1) end),
    }
  }

  -- Create the wibox
  s.mywibox = awful.wibar {
    position = "top",
    screen   = s,
    ontop    = false,
    margins  = beautiful.useless_gap + beautiful.border_width,
    border_width = 4,
    border_color = beautiful.bg_normal,
    widget   = {
      layout = wibox.layout.align.horizontal,
      expand = "none",
      { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        mylauncher,
        wibox.widget.textbox(' '),
        -- {
        --   {
        --     widget = s.mytaglist,
        --   },
        --   margins = 4,
        --   color = beautiful.secondary,
        --   widget = wibox.container.margin,
        -- },
        s.mytaglist,
        s.mypromptbox,
      },
      -- Middle widget
      wibox.container.margin(s.mytasklist, 5, 5, 5, 5),
      { -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        -- awful.widget.keyboardlayout(),
        wibox.widget.systray(),
        wibox.widget.textclock(),
        wibox.widget.textbox('| '),
        awful.widget.watch('power-man', 150, function(widget, stdout)
          for line in stdout:gmatch("[^\r\n]+") do
            widget:set_text(line..'%')
            return
          end
        end),
        wibox.widget.textbox('  '),
        s.mylayoutbox,
      },
    }
  }
end)
-- }}}

-- {{{ Mouse bindings
awful.mouse.append_global_mousebindings({
  awful.button({ }, 3, function() mymainmenu:toggle() end),
  -- awful.button({ }, 4, awful.tag.viewprev),
  -- awful.button({ }, 5, awful.tag.viewnext),
})
-- }}}

-- {{{ Key bindings

-- General
awful.keyboard.append_global_keybindings({
  awful.key({ modkey }, "s",
            hotkeys_popup.show_help,
            { description="show help", group="awesome" }),
  awful.key({ modkey }, "w",
            function() mymainmenu:show() end,
            { description="show main menu", group="awesome" }),
  awful.key({ modkey, "Shift" }, "r",
            awesome.restart,
            { description="reload awesome", group="awesome" }),
  awful.key({ modkey, "Control" }, "q",
            awesome.quit,
            { description="quit awesome", group="awesome" }),
  awful.key({ modkey, "Shift" }, "p",
            function()
              awful.prompt.run {
                prompt       = "Run Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
              }
            end,
            { description="lua execute prompt", group="awesome" }),
  awful.key({ modkey }, "Return",
            function() awful.spawn(terminal) end,
            { description="open a terminal", group="launcher" }),
  awful.key({ modkey }, "x",
            function() awful.spawn("betterlockscreen --lock dimblur --blur 8") end,
            { description="lock screen", group="awesome" }),
  awful.key({ modkey, "Shift" }, "c",
            function() awful.spawn("toggleprogram picom") end,
            { description="toggle compositor", group="awesome" }),
  awful.key({ modkey }, "d",
            function() awful.screen.focused().mypromptbox:run() end,
            { description="run prompt", group="launcher" }),
  awful.key({ modkey }, "r",
            function() menubar.show() end,
            { description="show the menubar", group="launcher" }),
})

-- Tag-related
awful.keyboard.append_global_keybindings({
  awful.key({ modkey }, "h",
            awful.tag.viewprev,
            { description="view previous", group="tag" }),
  awful.key({ modkey }, "l",
            awful.tag.viewnext,
            { description="view next", group="tag" }),
  awful.key({ modkey }, "Tab",
            awful.tag.history.restore,
            { description="go back", group="tag" }),
})

awful.keyboard.append_global_keybindings({
  awful.key({ modkey, "Shift" }, "e",
            function()
              local command = 'printf "No\nYes" | rofi -theme-str "configuration { fixed-num-lines: true; } listview { columns: 1; lines: 2; }" -dmenu -p "Exit awesome?"'
              awful.spawn.easy_async_with_shell(command, function(stdout, stderr, reason, exit_code)
                if stdout:sub(1, 3) == "Yes" then
                  awesome.quit()
                end
              end)
            end)
})

-- Focus-related
awful.keyboard.append_global_keybindings({
  -- awful.key({ modkey }, "j",
  --           function() awful.client.focus.byidx(1) end,
  --           { description="focus next by index", group="client" }),
  -- awful.key({ modkey }, "k",
  --           function() awful.client.focus.byidx(-1) end,
  --           {description="focus previous by index", group="client" }),
  -- awful.key({ modkey }, "Tab",
  --           function()
  --             awful.client.focus.history.previous()
  --             if client.focus then
  --               client.focus:raise()
  --             end
  --           end,
  --           { description="go back", group="client"}),
  -- awful.key({ modkey }, "Tab",
  --           function() awful.screen.focus_relative(1) end,
  --           { description="focus the next screen", group="screen" }),
  -- awful.key({ modkey, "Shift" }, "Tab",
  --           function() awful.screen.focus_relative(-1) end,
  --           { description="focus the previous screen", group="screen" }),
  awful.key({ modkey, "Shift" }, "-",
            function()
              local c = awful.client.restore()
              -- Focus restored client
              if c then
                c:activate { raise=true, context="key.unminimize" }
              end
            end,
            { description="restore minimized", group="client" }),
})

-- Layout
awful.keyboard.append_global_keybindings({
  -- awful.key({ modkey, "Shift" }, "j",
  --           function() awful.client.swap.byidx(1) end,
  --           { description="swap with next client by index", group="client" }),
  -- awful.key({ modkey, "Shift" }, "k",
  --           function() awful.client.swap.byidx(-1) end,
  --           { description="swap with previous client by index", group="client" }),
  -- awful.key({ modkey }, "u",
  --           awful.client.urgent.jumpto,
  --           { description="jump to urgent client", group="client" }),
  -- awful.key({ modkey, "Control" }, "Right",
  --           function() awful.tag.incmwfact(0.05) end,
  --           { description="increase master width factor", group="layout" }),
  -- awful.key({ modkey, "Control" }, "Left",
  --           function() awful.tag.incmwfact(-0.05) end,
  --           { description="decrease master width factor", group="layout" }),
  awful.key({ modkey }, "m",
            function() awful.tag.incnmaster(1, nil, true) end,
            { description="increase the number of master clients", group="layout" }),
  awful.key({ modkey, "Shift" }, "m",
            -- TODO Don't decrease masters to 0
            function() awful.tag.incnmaster(-1, nil, true) end,
            { description="decrease the number of master clients", group="layout" }),
  awful.key({ modkey }, "v",
            function() awful.tag.incncol(1, nil, true) end,
            { description="increase the number of columns", group="layout" }),
  awful.key({ modkey, "Shift" }, "v",
            function() awful.tag.incncol(-1, nil, true) end,
            { description="decrease the number of columns", group="layout" }),
  awful.key({ modkey }, "space",
            function() awful.layout.inc(1) end,
            { description="select next", group="layout" }),
  awful.key({ modkey, "Shift" }, "space",
            function() awful.layout.inc(-1) end,
            { description="select previous", group="layout" }),
})

awful.keyboard.append_global_keybindings({
  awful.key({}, "XF86AudioPlay",
             function() awful.util.spawn("playerctl play-pause") end),
  awful.key({}, "XF86AudioPause",
             function() awful.util.spawn("playerctl play-pause") end),
  awful.key({}, "XF86AudioNext",
             function() awful.util.spawn("playerctl next") end),
  awful.key({}, "XF86AudioPrev",
             function() awful.util.spawn("playerctl previous") end),
  awful.key({}, "XF86AudioLowerVolume",
             function() awful.util.spawn("pactl set-sink-volume 2 -5%") end),
  awful.key({}, "XF86AudioRaiseVolume",
            function() awful.util.spawn("pactl set-sink-volume 2 +5%") end),
  awful.key({}, "XF86AudioMute",
            function() awful.util.spawn("amixer -q -D pulse sset Master toggle") end),
})

--[[-CHARITABLE
local tableLength = function(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end
--]]

awful.keyboard.append_global_keybindings({
  awful.key {
    modifiers   = { modkey },
    keygroup    = "numrow",
    description = "only view tag",
    group       = "tag",
    on_press    = function(i)
      -- [[+CHARITABLE
      -- TODO Raise tag after swapping
      -- TODO Tag history toggle with charitable
      local tag = tags[i]
      if tag then
        charitable.select_tag(tag, awful.screen.focused())
      end
      --]]
      --[[-CHARITABLE
      local screen = awful.screen.focused()
      local tag = screen.tags[i]
      local tags = awful.screen.focused().selected_tags
      local tagslen = tableLength(tags)
      if tag then
        -- If attempting to focus current tag, focus previous from history
        if tagslen == 1 and tag.name == tags[1].name then
          awful.tag.history.restore()
        else
          tag:view_only()
        end
      end
      --]]
    end,
  },
  awful.key {
    modifiers   = { modkey, "Control" },
    keygroup    = "numrow",
    description = "toggle tag",
    group       = "tag",
    on_press    = function(i)
      -- [[+CHARITABLE
      local tag = tags[i]
      if tag then
        charitable.toggle_tag(tag, awful.screen.focused())
      end
      --]]
      --[[-CHARITABLE
      local screen = awful.screen.focused()
      local tag = screen.tags[i]
      if tag then
        awful.tag.viewtoggle(tag)
      end
      --]]
    end,
  },
  awful.key {
    modifiers = { modkey, "Shift" },
    keygroup    = "numrow",
    description = "move focused client to tag",
    group       = "tag",
    on_press    = function(i)
      if client.focus then
        -- local tag = client.focus.screen.tags[i]
        local tag = tags[i]
        if tag then
          client.focus:move_to_tag(tag)
        end
      end
    end,
  },
  awful.key {
    modifiers   = { modkey, "Control", "Shift" },
    keygroup    = "numrow",
    description = "toggle focused client on tag",
    group       = "tag",
    on_press    = function(i)
      if client.focus then
        -- local tag = client.focus.screen.tags[i]
        local tag = tags[i]
        if tag then
          client.focus:toggle_tag(tag)
        end
      end
    end,
  },
  awful.key {
    modifiers   = { modkey },
    keygroup    = "numpad",
    description = "select layout directly",
    group       = "layout",
    on_press    = function(i)
      local t = awful.screen.focused().selected_tag
      if t then
        t.layout = t.layouts[i] or t.layout
      end
    end,
  }
})

awful.keyboard.append_client_keybindings({
  -- Resize windows
  awful.key({ modkey, "Control" }, "Up",
            function(c)
              if c.floating then
                c:relative_move(0, 0, 0, -10)
              else
                awful.client.incwfact(0.025)
              end
            end,
            { description="Resize Vertical -", group="client" }),
  awful.key({ modkey, "Control" }, "Down",
            function(c)
              if c.floating then
                c:relative_move(0, 0, 0, 10)
              else
                awful.client.incwfact(-0.025)
              end
            end,
            { description="Resize Vertical +", group="client" }),
  awful.key({ modkey, "Control" }, "Left",
            function(c)
              if c.floating then
                c:relative_move(0, 0, -10, 0)
              else
                awful.tag.incmwfact(-0.025)
              end
            end,
            { description="Resize Horizontal -", group="client" }),
  awful.key({ modkey, "Control" }, "Right",
            function(c)
              if c.floating then
                c:relative_move( 0, 0,  10, 0)
              else
                awful.tag.incmwfact(0.025)
              end
            end,
            { description="Resize Horizontal +", group="client" }),

  -- Moving windows
  awful.key({ modkey, "Shift" }, "Down",
            function(c)
              if c.floating then
                c:relative_move(0, 20, 0, 0)
              else
                awful.client.swap.global_bydirection("down")
                c:raise()
              end
            end,
            { description="Move Down", group="client" }),
  awful.key({ modkey, "Shift" }, "Up",
            function(c)
              if c.floating then
                c:relative_move(0, -20, 0, 0)
              else
                awful.client.swap.global_bydirection("up")
                c:raise()
              end
            end,
            { description="Move Up", group="client" }),
  awful.key({ modkey, "Shift" }, "Left",
            function(c)
              if c.floating then
                c:relative_move(-20, 0, 0, 0)
              else
                awful.client.swap.global_bydirection("left")
                c:raise()
              end
            end,
            { description="Move Left", group="client" }),
  awful.key({ modkey, "Shift" }, "Right",
            function(c)
              if c.floating then
                c:relative_move(20, 0, 0, 0)
              else
                awful.client.swap.global_bydirection("right")
                c:raise()
              end
            end,
            { description="Move Right", group="client" }),
  awful.key({ modkey }, "Down",
            function(c)
              awful.client.focus.global_bydirection("down")
              if c.focus then c.focus:raise() end
            end,
            { description="focus next window up", group="client" }),
  awful.key({ modkey }, "Up",
            function(c)
              awful.client.focus.global_bydirection("up")
              if c.focus then c.focus:raise() end
            end,
            { description="focus next window down", group="client" }),
  awful.key({ modkey }, "Right",
            function(c)
              awful.client.focus.global_bydirection("right")
              if c.focus then c.focus:raise() end
            end,
            { description="focus next window right", group="client" }),
  awful.key({ modkey }, "Left",
            function(c)
              awful.client.focus.global_bydirection("left")
              if c.focus then c.focus:raise() end
            end,
            { description="focus next window left", group="client" }),
  awful.key({ modkey }, "n",
            function() scratch.toggle("alacritty --class scratch --title scratchpad --command tmux new-session -A -s scratchpad", {instance="scratch"}) end,
            { description="toggle scratchpad", group="client" }),
  awful.key({ modkey, "Shift" }, "n",
            function() scratch.toggle("alacritty --class math --title math --option font.size=18 --command tmux new-session -A -s math python3", {instance="math"}) end,
            { description="toggle math scratchpad", group="client" }),
})

client.connect_signal("request::default_mousebindings", function()
  awful.mouse.append_client_mousebindings({
    awful.button({ }, 1,
                 function(c) c:activate { context="mouse_click" } end),
    awful.button({ modkey }, 1,
                 function(c) c:activate { context="mouse_click", action="mouse_move" } end),
    awful.button({ modkey }, 3,
                 function(c) c:activate { context="mouse_click", action="mouse_resize" } end),
  })
end)

client.connect_signal("request::default_keybindings", function()
  awful.keyboard.append_client_keybindings({
    awful.key({ modkey }, "f",
              function(c)
                c.fullscreen = not c.fullscreen
                c:raise()
              end,
              { description="toggle fullscreen", group="client" }),
    awful.key({ modkey, "Shift" }, "f",
              awful.client.floating.toggle,
              { description="toggle floating", group="client" }),
    awful.key({ modkey }, "q",
              function(c) c:kill() end,
              { description="close", group="client" }),
    awful.key({ modkey, "Shift" }, "Return",
              function(c) c:swap(awful.client.getmaster()) end,
              { description="move to master", group="client" }),
    -- awful.key({ modkey }, "o",
    --           function(c) c:move_to_screen() end,
    --           { description="move to screen", group="client" }),
    awful.key({ modkey }, ".",
              function(c) c.ontop = not c.ontop end,
              { description="toggle keep on top", group="client" }),
    awful.key({ modkey }, "-",
                -- The client currently has the input focus, so it cannot be
                -- minimized, since minimized clients can't have the focus.
              function(c) c.minimized = true end,
              { description="minimize", group="client" }),
    -- awful.key({ modkey }, "f",
    --           function(c)
    --             c.maximized = not c.maximized
    --             c:raise()
    --           end,
    --           { description="(un)maximize", group="client" }),
  })
end)

-- }}}

--[[{{{ Rules
https://awesomewm.org/doc/api/libraries/awful.rules.html
https://www.reddit.com/r/awesomewm/comments/mytkwa/awfulrules_regex_wildcards/
xprop ->
  WM_CLASS[1] => instance
  WM_CLASS[2] => class
--]]

ruled.client.connect_signal("request::rules", function()
  -- All clients will match this rule
  ruled.client.append_rule {
    id = "global",
    rule = { },
    properties = {
      focus = awful.client.focus.filter,
      raise = true,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen,
    }
  }

  -- Floating clients
  ruled.client.append_rule {
    id = "floating",
    rule_any = {
      instance = { "copyq", "pinentry" },
      class = {
        "Arandr", "Blueman-manager", "Gpick", "Kruler", "Sxiv",
        "Tor Browser", "Wpa_gui", "veromix", "xtightvncviewer"
      },
      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name = {
        "Event Tester",  -- xev.
      },
      role = {
        "AlarmWindow",    -- Thunderbird's calendar.
        "ConfigManager",  -- Thunderbird's about:config.
        "pop-up",         -- e.g. Google Chrome's (detached) Developer Tools.
      }
    },
    properties = { floating=true }
  }

  -- Add titlebars to normal clients and dialogs
  ruled.client.append_rule {
    id         = "titlebars",
    rule_any   = { type = { "normal", "dialog" } },
    properties = { titlebars_enabled=true }
  }

  ruled.client.append_rule {
    rule = { name="Remmina Remote Desktop Client" },
    properties = { floating=true, ontop=true, width=800, height=600 }
  }

  ruled.client.append_rule {
    rule = { class="Nm-connection-editor" },
    properties = { floating=true, ontop=true, width=600, height=800 }
  }
  ruled.client.append_rule {
    rule = { class="TelegramDesktop" },
    properties = { floating=true, ontop=true, width=1200, height=750 }
  }
  ruled.client.append_rule {
    rule = { class="PrimeNote" },
    except = { name="PrimeNote Settings" },
    properties = { floating=true, ontop=true, width=400, height=320, titlebars_enabled=false, sticky=true, above=true, border_width=0, skip_taskbar=true, honor_padding=false, honor_workarea=true, minimized=false, request_no_titlebar=true }
  }
  ruled.client.append_rule {
    rule_any = { name={"Office 365 Mailbox.*", "Security Group.*"} },
    properties = { floating=true }
  }
  -- TODO Cleaner rule-function rule("class" or {name="name"}, {floating=true})

  -- TODO Move to own file
  local ws = function(opts)
    setmetatable(opts, {__index={ tag=1, classes={""} }})
    local tag, classes =
      opts[1] or opts.tag,
      opts[2] or opts.classes

    ruled.client.append_rule {
      rule_any = {
        class = classes
      },
      properties = { tag=tags[tag] }
    }
  end

  ws {2,{
    "Mattermost",
    "discord",
    "TelegramDesktop",
  }}
  ws {5, "org.remmina.Remmina"}
  ws {9, "KeePassXC"}

  local scratch_props = {
    floating=true,
    titlebars_enabled=true,
    minimized=true,
    -- width=1300,
    -- height=900,
    sticky=false,
    above=true,
    ontop=false,
    border_width=0,
    skip_taskbar=true,
    honor_padding=true,
    honor_workarea=true
  }

  ruled.client.append_rule {
    rule       = { instance="scratch" },
    properties = scratch_props
  }
  ruled.client.append_rule {
    rule       = { instance="math" },
    properties = scratch_props
  }
end)

-- }}}

-- {{{ Titlebars
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    awful.button({ }, 1,
                 function() c:activate { context="titlebar", action="mouse_move" } end),
    awful.button({ }, 3,
                 function() c:activate { context="titlebar", action="mouse_resize" } end),
  }

  awful.titlebar(c).widget = {
    {
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    {
      {
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    {
      -- awful.titlebar.widget.floatingbutton (c),
      awful.titlebar.widget.minimizebutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      -- awful.titlebar.widget.stickybutton   (c),
      -- awful.titlebar.widget.ontopbutton    (c),
      awful.titlebar.widget.closebutton    (c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
end)

-- {{{ Notifications

ruled.notification.connect_signal('request::rules', function()
  -- All notifications will match this rule.
  ruled.notification.append_rule {
    rule       = { },
    properties = {
      screen           = awful.screen.preferred,
      implicit_timeout = 5,
    }
  }
end)

naughty.connect_signal("request::display", function(n)
  naughty.layout.box { notification=n }
end)

-- }}}

-- [[+CHARITABLE
-- Ensure that removing screens doesn't kill tags
tag.connect_signal("request::screen", function(t)
    t.selected = false
    for s in capi.screen do
        if s ~= t.screen then
            t.screen = s
            return
        end
    end
end)

-- work around bugs in awesome 4.0 through 4.3+
-- see https://github.com/awesomeWM/awesome/issues/2780
awful.tag.history.restore = function() end
--]]

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  c:activate { context="mouse_enter", raise=false }
end)

--[[ Floating node behaviors
local floatnode = function(c)
  -- TODO c.first_tag equivalent with tags[]
    c.ontop = true
    awful.titlebar.show(c)
end
local unfloatnode = function(c)
    c.ontop = false
    awful.titlebar.hide(c)
end

client.connect_signal("property::floating", function(c)
  if c.floating then
    floatnode(c)
  else
    unfloatnode(c)
  end
end)
client.connect_signal("manage", function(c)
  if c.floating or c.first_tag.layout.name == "floating" then
    floatnode(c)
  else
    unfloatnode(c)
  end
end)
tag.connect_signal("property::layout", function(t)
  local clients = t:clients()
  for k,c in pairs(clients) do
    if c.floating or c.first_tag.layout.name == "floating" then
      floatnode(c)
    else
      unfloatnode(c)
    end
  end
end)
--]]

client.connect_signal("manage", function (c)
  -- TODO Spawn below current node, not the overall slave
  -- if not awesome.startup then awful.client.setslave(c) end

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

  if awesome.startup
    and not c.size_hints.user_position
    and not c.size_hints.program_position then
      -- Prevent clients from being unreachable after screen count changes.
      awful.placement.no_offscreen(c)
  end
end)

--[[ Remove border when only one client
screen.connect_signal("arrange", function (s)
    local only_one = #s.tiled_clients == 1
    for _, c in pairs(s.clients) do
        if only_one and not c.floating or c.maximized then
            c.border_width = 0
        else
            c.border_width = beautiful.border_width
        end
    end
end)
--]]
-- }}}
