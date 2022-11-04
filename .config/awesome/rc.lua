--[[ Resources {{{
 - /etc/xdg/awesome/rc.lua
 - https://awesomewm.org/apidoc/
 - https://github.com/atsepkov/awesome-awesome-wm
 - Convert unicode character to png icon:
   $ convert -background transparent -fill '#E6E6E6' -font ~/.fonts/JetBrains/JetBrains\ Mono\ Bold\ Italic\ Nerd\ Font\ Complete\ Mono.ttf -pointsize 48 -size 64x64 -gravity center label:TEXT output.png
 - Debug lua:
   $ awesome-client 'return require("gears").filesystem.get_configuration_dir()'
 - https://stackoverflow.com/questions/69574689/how-to-limit-the-width-of-window-entries-on-the-wibar
 - https://github.com/mut-ex/awesome-wm-nice
 - https://www.reddit.com/r/awesomewm/comments/gehk1g/cursor_follows_focus_possible/
 - https://awesomewm.org/recipes/
 - https://www.reddit.com/r/awesomewm/comments/mfklx5/shadow_only_for_floating_windows/
 - https://www.reddit.com/r/awesomewm/comments/bki1md/show_titlebar_only_when_window_is_floating/
 - https://www.reddit.com/r/awesomewm/comments/box4jk/my_functional_dynamic_border_gap_and_titlebar/
--}}}]]
-- TODO focus_relative to force focus to screen

require("signals")
-- Packages {{{
pcall(require, "luarocks.loader")

local shape = require("gears.shape")
-- Dynamic tags
require("modules.eminent.eminent")
-- XMonad-like workspaces
local charitable    = require("modules.charitable")
-- OSX-like titlebars, for some reason can't be required from a subdirectory (modules)
-- local nice          = require("nice")
-- local machi         = require("modules.machi")

-- Client movement handling
-- require("collision")()

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
local inspect       = require("modules.inspect.inspect")

unpack = table.unpack or unpack
local ez            = require("modules.ez")
local H = require("helpers")
-- }}}

local dovetail = require("modules.dovetail")

-- local CFloating = function(c)
--    return c.floating or awful.layout.get(c.screen) == awful.layout.suit.floating
-- end

-- Variables {{{
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")
local bling = require("modules.bling")
local rubato = require("modules.rubato")
bling.module.flash_focus.enable()
bling.module.window_swallowing.start()

-- local anim_y = rubato.timed {
--     pos = -1450,
--     rate = 144,
--     easing = rubato.quadratic,
--     intro = 0.1,
--     duration = 0.3,
--     awestore_compat = true
-- }
local term_scratch = bling.module.scratchpad {
  -- command = "wezterm connect scratch --class scratch",
  command = "alacritty --class scratch --option font.size=12 --command tmux new-session -A -s scratch",
  rule = { instance="scratch" },
  sticky = true,
  autoclose = false,
  floating = true,
  geometry = { x=0, y=0, height=800, width=1200 },
  reapply = false,
  dont_focus_before_close = true,
  -- rubato = { y=anim_y }
}
local qalc_scratch = bling.module.scratchpad {
  command = "qalculate-gtk",
  rule = { class="Qalculate-gtk" },
  sticky = true,
  autoclose = false,
  floating = true,
  geometry = { x=0, y=0, height=730, width=730 },
  reapply = false,
  dont_focus_before_close = true,
}
-- term_scratch:connect_signal("inital_apply", function(c)
--   awful.placement.centered(c)
--   awful.placement.no_overlap(c)
--   awful.placement.no_offscreen(c)
-- end)

-- WARN: nice breaks with gtk4, temporary workaround is to uninstall it - https://github.com/mut-ex/awesome-wm-nice/issues/22
--[[+NICE
nice {
  -- To disable titlebars, set titlebar_height and titlebar_radius to 3,
  -- and remove the titlebar_items or set button_size to 0.
  -- titlebar_color = beautiful.bg
  --titlebar_height = 3,
  --titlebar_radius = 3,
  --button_size = 0,
  --titlebar_items = { left={}, middle={}, right={} },
  titlebar_height = 28,
  titlebar_radius = 9,
  button_size = 12,
  titlebar_font = beautiful.font,
  win_shade_enabled = false,
  no_titlebar_maximized = false,
  titlebar_items = {
    -- {"sticky", "ontop", "floating"},
    left = {"close", "minimize", "maximize"},
    middle = "title",
    right = {}
  }
}
--]]

-- awesome.set_preferred_icon_size(32)

awful.mouse.snap.edge_enabled = false

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

modkey = "Mod4"
-- }}}

-- Menu {{{
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
-- machi.editor.nested_layouts = {
--   ["1"] = awful.layout.suit.fair,
--   ["2"] = bling.layout.deck,
-- }
-- machi.layout.default_cmd="11h0x1x"

-- Layouts {{{
tag.connect_signal("request::default_layouts", function()
  awful.layout.append_default_layouts({
    awful.layout.suit.tile,
    awful.layout.suit.max,
    bling.layout.centered,
    -- machi.default_layout,
    dovetail.layout.right,
    -- awful.layout.suit.max,
  })
end)
-- }}}

-- Wallpaper {{{
screen.connect_signal("request::wallpaper", function(s)
  local wall_cmds = {
    "nitrogen --restore",
    (os.getenv("HOME") or "~") .. "/.fehbg",
    "feh --bg-scale " .. gears.filesystem.get_xdg_config_home() .. "wall.jpg",
  }

  setwall = function(cmds, i)
    if i > #cmds then
      awful.wallpaper {
        screen = s,
        bg = "#000000",
        widget = {
          image                 = beautiful.wallpaper,
          upscale               = true,
          downscale             = true,
          resize                = false,
          horizontal_fit_policy = "fit",
          vertical_fit_policy   = "auto",
          halign                = "center",
          valign                = "center",
          widget                = wibox.widget.imagebox
        }
      }
      return
    end

    awful.spawn.easy_async_with_shell(cmds[i], function(_, _, _, retcode)
      if retcode ~= 0 then
        setwall(cmds, i + 1)
      end
    end)
  end

  setwall(wall_cmds, 1)
end)
-- }}}

--[[ Wibar {{{
https://awesomewm.org/apidoc/popups_and_bars/awful.wibar.html 
--]]

-- [[+CHARITABLE
local tags = charitable.create_tags(
  -- { "  ", "  ", "  ", " ﮑ ", " 龎 ", " ﮠ ", "  ", " 煉 ", "  " },
  { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
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
    awful.layout.layouts[1],
  }
)
--]]

screen.connect_signal("request::desktop_decoration", function(s)
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
      -- awful.button({}, 1, function(t) charitable.select_tag(t, awful.screen.focused()) end),
      awful.button({}, 1, function(t)
        if awful.screen.focused().selected_tag == t then
          awful.tag.history.restore()
        else
          charitable.select_tag(t, awful.screen.focused())
        end
      end),
      awful.button({}, 3, function(t) charitable.toggle_tag(t, awful.screen.focused()) end),
      -- TODO Improve view{next,prev}
      awful.button({}, 4, function(t) awful.tag.viewprev(t.screen) end),
      awful.button({}, 5, function(t) awful.tag.viewnext(t.screen) end)
    ),
    source = function(screen, args) return tags end,
  })

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox {
    screen  = s,
    buttons = {
      awful.button({}, 1, function() awful.layout.inc(1) end),
      awful.button({}, 3, function() awful.layout.inc(-1) end),
      awful.button({}, 4, function() awful.layout.inc(-1) end),
      awful.button({}, 5, function() awful.layout.inc(1) end),
    }
  }

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist {
    screen  = s,
    filter  = awful.widget.tasklist.filter.currenttags,
    buttons = {
      awful.button({}, 1, function(c) c:activate { context = "tasklist", action = "toggle_minimization" } end),
      awful.button({}, 3, function() awful.menu.client_list { theme = { width = 250 } } end),
      awful.button({}, 4, function() awful.client.focus.byidx(-1) end),
      awful.button({}, 5, function() awful.client.focus.byidx(1) end),
    }
  }

  -- Create the wibox
  s.mywibox = awful.wibar {
    position     = "top",
    screen       = s,
    ontop        = false,
    margins      = beautiful.useless_gap + beautiful.border_width,
    border_width = 4,
    border_color = beautiful.bg_normal,
    widget       = {
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
        {
          layout = wibox.layout.flex.horizontal,
          {
            align = "center",
            widget = awful.widget.watch('power-man', 150, function(widget, stdout)
              for line in stdout:gmatch("[^\r\n]+") do
                widget:set_text(line .. '%')
                return
              end
            end),
          },
          buttons = {
            awful.button({}, 4, function() awful.spawn("xbacklight -inc 5") end),
            awful.button({}, 5, function() awful.spawn("xbacklight -dec 5") end),
          }
        },
        wibox.widget.textbox('  '),
        s.mylayoutbox,
      },
    }
  }
end)
-- }}}

cache = {} --[[cache]]

-- Key bindings (`xev`) {{{
local globalkeys = ez.keytable {
  ["M-S-r"] = awesome.restart,
  ["M-C-q"] = awesome.quit,
  ["M-Return"] = {awful.spawn, terminal},
  ["A-p"] = {awful.util.spawn, "screenshot -m region -t -c -o 'screenshot-xbackbone'"},
  ["A-S-p"] = {awful.util.spawn, "screenshot -m region -c"},
  ["M-x"] = {awful.spawn, "betterlockscreen --lock dimblur --blur 8"},
  ["M-S-c"] = {awful.spawn, "toggleprogram picom"},
  ["M-d"] = {awful.spawn, "rofi -show run"},
  ["M-h"] = awful.tag.viewprev,
  ["M-l"] = awful.tag.viewnext,
  ["M-Tab"] = awful.tag.history.restore,
  ["M-S-e"] = function()
    local command = 'printf "No\nYes" | rofi -theme-str "configuration { fixed-num-lines: true; } listview { columns: 1; lines: 2; }" -dmenu -p "Exit awesome?"'
    awful.spawn.easy_async_with_shell(command, function(stdout,_,_,_)
      if stdout:sub(1, 3) == "Yes" then
        awesome.quit()
      end
    end)
  end,
  ["M-t"] = function(--[[cache]])
    if not cache.l_lay then cache.l_lay = {} end
    local t = awful.screen.focused().selected_tag
    local de = awful.layout.layouts[awful.layout.get_tag_layout_index(t)]
    local ds = awful.screen.focused().selected_tag.index
    local dx = awful.screen.focused().index
    dk = ds .."-".. dx
    if not string.match(awful.layout.getname(), 'max') then
      cache.l_lay[dk] = de
      awful.layout.set(awful.layout.suit.max)
    else
      awful.layout.set(cache.l_lay[dk])
    end
  end,
  ["M-m"] = {awful.tag.incnmaster, 1, nil, true},
  ["M-S-m"] = function()
    local masters = awful.tag.getnmaster() -- awful.screen.focused().selected_tag.master_count
    if masters > 1 then
      awful.tag.incnmaster(-1, nil, true)
    end
  end,
  ["M-v"] = {awful.tag.incncol, 1, nil, true},
  ["M-S-v"] = {awful.tag.incncol, -1, nil, true},
  ["M-space"] = {awful.layout.inc, 1},
  ["M-S-space"] = {awful.layout.inc, -1},
  ["XF86AudioPlay"] = {awful.util.spawn, "playerctl play-pause"},
  ["XF86AudioPause"] = {awful.util.spawn, "playerctl play-pause"},
  ["XF86AudioNext"] = {awful.util.spawn, "playerctl next"},
  ["XF86AudioPrev"] = {awful.util.spawn, "playerctl previous"},
  ["XF86AudioLowerVolume"] = {awful.util.spawn, "pactl set-sink-volume 2 -5%"},
  ["XF86AudioRaiseVolume"] = {awful.util.spawn, "pactl set-sink-volume 2 +5%"},
  ["XF86AudioMute"] = {awful.util.spawn, "amixer -q -D pulse sset Master toggle"},
  ["M-n"] = function() term_scratch:toggle() end,
  ["M-p"] = function() qalc_scratch:toggle() end,
  ["M-period"] = function(--[[cache]]) -- Toggle adhoc-scratchpads
    if not cache.adhoc then return end

    for _,scratch in pairs(cache.adhoc) do
      scratch:toggle()
    end
  end,
  ["M-S-period"] = function()
    local c = awful.client.restore()
    if c then
      c:activate { raise=true, context="key.unminimize" }
    end
  end,
}

local clientkeys = ez.keytable {
  -- ["M-o"] = function(c) end,
  ["M-comma"] = function(c --[[cache]]) -- Tag a client as an adhoc-scratchpad
    -- TODO Make scratchpad float, then restore after re-assign
    if not c then return end
    -- if not cache.adhoc then cache.adhoc = {} end -- Support tagging multiple clients
    cache.adhoc = {} -- Only support tagging one client
    if H.THas(cache.adhoc, c.pid) then return end

    cache.adhoc[c.pid] = bling.module.scratchpad {
      command = "notify-send -t 1000 'AwesomeWM ad-hoc scratchpads' 'Cannot restart scratchpad for "..c.name.." ("..c.class..")'",
      rule = { pid=c.pid },
      sticky = false,
      autoclose = false,
      reapply = false,
      dont_focus_before_close = true,
    }
    -- cache.adhoc[c.pid]:connect_signal("turn_off", function(c) H.debug("Turned off!") end)
  end,
  -- ["M-S-comma"] = function(c --[[cache]]) -- Untag a client as an adhoc-scratchpad
  --   if not cache.adhoc then return end
  --   cache.adhoc[c.pid]:turn_on()
  --   cache.adhoc[c.pid] = nil
  -- end,
  ["M-minus"] = function(c) c.minimized = true end,
  ["M-f"] = function(c)
    c.fullscreen = not c.fullscreen
    c:raise()
  end,
  ["M-S-f"] = function(c)
    if not c.fullscreen then
      -- awful.client.floating.toggle,
      c.floating = not c.floating
      c:raise()
    end
  end,
  ["M-q"] = function(c) c:kill() end,
  ["M-S-q"] = {awful.spawn, "xkill"},
  ["M-S-Return"] = function(c) c:swap(awful.client.getmaster()) end,
  -- ["M-period"] = function(c) c.ontop = not c.ontop end,
  -- ["M-period"] = function() machi.default_editor.start_interactive() end,
  -- ["M-comma"] = function() machi.switcher.start(client.focus) end,
  ["M-C-Up"] = function(c)
    if c.floating then
      c:relative_move(0, 0, 0, -50)
    else
      awful.client.incwfact(0.025)
    end
  end,
  ["M-C-Down"] = function(c)
    if c.floating then
      c:relative_move(0, 0, 0, 50)
    else
      awful.client.incwfact(-0.025)
    end
  end,
  ["M-C-Left"] = function(c)
    if c.floating then
      c:relative_move(0, 0, -50, 0)
    else
      awful.tag.incmwfact(-0.025)
    end
  end,
  ["M-C-Right"] = function(c)
    if c.floating then
      c:relative_move(0, 0, 50, 0)
    else
      awful.tag.incmwfact(0.025)
    end
  end,
  ["M-S-Down"] = function(c)
    if c.floating then
      c:relative_move(0, 50, 0, 0)
    else
      awful.client.swap.global_bydirection("down")
      c:raise()
    end
  end,
  ["M-S-Up"] = function(c)
    if c.floating then
      c:relative_move(0, -50, 0, 0)
    else
      awful.client.swap.global_bydirection("up")
      c:raise()
    end
  end,
  ["M-S-Left"] = function(c)
    if c.floating then
      c:relative_move(-50, 0, 0, 0)
    else
      awful.client.swap.global_bydirection("left")
      c:raise()
    end
  end,
  ["M-S-Right"] = function(c)
    if c.floating then
      c:relative_move(50, 0, 0, 0)
    else
      awful.client.swap.global_bydirection("right")
      c:raise()
    end
  end,
  ["M-Down"] = function(c)
    -- https://www.reddit.com/r/awesomewm/comments/j73j99/comment/g82ik6f
    -- awful.screen.focused().select_tag vs mouse.screen.selected_tag
    if H.THas({"dovetail.layout.right", "max"}, awful.screen.focused().selected_tag.layout.name) then
      awful.client.focus.byidx(1)
      -- Buggy when a floating client exists
      if client.focus == awful.client.getmaster(awful.screen.focused()) then
        awful.client.focus.byidx(-1)
      end
    else
      awful.client.focus.global_bydirection("down")
    end
  end,
  ["M-Up"] = function(c)
    if H.THas({"dovetail.layout.right", "max"}, awful.screen.focused().selected_tag.layout.name) then
      if client.focus ~= awful.client.getmaster(awful.screen.focused()) then
        awful.client.focus.byidx(-1)
      end
    else
      awful.client.focus.global_bydirection("up")
    end
  end,
  ["M-Right"] = function(c)
    awful.client.focus.global_bydirection("right")
  end,
  ["M-Left"] = function(c)
    awful.client.focus.global_bydirection("left")
  end,
}

local numberkeys = {
  awful.key {
    modifiers   = { modkey },
    keygroup    = "numrow",
    description = "only view tag",
    group       = "tag",
    on_press    = function(i)
      -- TODO Raise tag after swapping
      -- TODO Tag history toggle with charitable
      if i==0 then i=10 end
      local tag = tags[i]
      if tag then
        if H.IndexOf(tags, awful.screen.focused().selected_tag) == i then
          awful.tag.history.restore()
        else
          charitable.select_tag(tag, awful.screen.focused())
        end
      end
    end,
  },
  awful.key {
    modifiers   = { modkey, "Control" },
    keygroup    = "numrow",
    description = "toggle tag",
    group       = "tag",
    on_press    = function(i)
      if i==0 then i=10 end
      local tag = tags[i]
      if tag then
        charitable.toggle_tag(tag, awful.screen.focused())
      end
    end,
  },
  awful.key {
    modifiers   = { modkey, "Shift" },
    keygroup    = "numrow",
    description = "move focused client to tag",
    group       = "tag",
    on_press    = function(i)
      if client.focus then
        if i==0 then i=10 end
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
        if i==0 then i=10 end
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
}

client.connect_signal("request::default_keybindings", function()
  awful.keyboard.append_global_keybindings(globalkeys)
  awful.keyboard.append_client_keybindings(clientkeys)
  awful.keyboard.append_global_keybindings(numberkeys)
end)

local globalbuttons = ez.btntable {
  ["3"] = function() mymainmenu:toggle() end,
}

local clientbuttons = ez.btntable {
  ["1"] = function(c) client.focus = c end,
  ["M-1"] = awful.mouse.client.move,
  ["M-3"] = awful.mouse.client.resize,
}

client.connect_signal("request::default_mousebindings", function()
  awful.mouse.append_global_mousebindings(globalbuttons)
  awful.mouse.append_client_mousebindings(clientbuttons)
end)
-- }}}

-- Rules {{{
-- https://awesomewm.org/doc/api/libraries/awful.rules.html

-- This is a pseudo-rule workaround for tag-rules causing a crash when using chariatable / sharedtags.
local rule = function(rules, clients)
  -- Rules act as rule_any
  -- (2 -> {["tag"]=tags[2]})
  if type(rules) ~= "table" then
    rules = { tag=tags[rules] }
  end
  -- ("firefox" -> {"firefox"})
  if type(clients) ~= "table" then
    clients = { clients }
  end

  -- When the client requests a class
  client.connect_signal("property::class", function(c)
    -- Loop through all clients the rule(s) should apply to
    for client_k, client_v in pairs(clients) do
      -- "number" in this sense means that the key is undefined, so we assume it should be a class
      -- ([1]="firefox" -> ["class"]="firefox")
      if type(client_k) == "number" then
        client_k = "class"
      end

      -- Check if the rule matches the client
      if (client_k == "class"    and string.match(c.class,    client_v)) or
         (client_k == "instance" and string.match(c.instance, client_v)) then
        -- Loop through all rules
        for rule_k, rule_v in pairs(rules) do
          -- Apply rules
          if rule_k == "tag" then
            c:move_to_tag(rule_v)
            -- charitable.select_tag(rule_v, awful.screen.focused())
          end
          -- if rule_k == "width" then
          --   c:relative_move(0, 0, rule_v, 0)
          -- end
        end
      end
    end
  end)
end

ruled.client.connect_signal("request::rules", function()
  rule(2, {
    "Mattermost",
    "TelegramDesktop",
    "discord",
    "Microsoft Teams - Preview",
  })
  rule(3, "Code")
  rule(5, "org.remmina.Remmina")
  rule(6, "Inkscape")
  rule(8, "[Ss]potify")
  rule(10, "KeePassXC")
  -- TODO eww
  -- rule({border_width=0}, "eww-bar")

  -- All clients will match this rule
  ruled.client.append_rule {
    id = "global",
    rule = {},
    properties = {
      focus = awful.client.focus.filter,
      raise = true,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap + awful.placement.no_offscreen,
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
        "Event Tester", -- xev.
      },
      role = {
        "AlarmWindow", -- Thunderbird's calendar.
        "ConfigManager", -- Thunderbird's about:config.
        "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
        "Popup" -- Firefox
      }
    },
    properties = { floating=true, border_width=0, above=true }
  }

  ruled.client.append_rule {
    rule_any   = { class={"Fig"}, instance={"fig"}, role={"autocomplete"} },
    properties = { titlebars_enabled=false, border_width=0, focus=false, above=true }
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
    rule_any = { name={ "Office 365 Mailbox.*", "Security Group.*" } },
    properties = { floating=true }
  }
  ruled.client.append_rule {
    rule = { class="Pavucontrol" },
    properties = { floating=true }
  }

  -- Fix unresizable electron windows like Discord
  ruled.client.append_rule {
    rule_any = { role={ "browser-window" } },
    properties = { is_fixed=true, size_hints_honor=false }
  }

  ruled.client.append_rule {
    rule_any   = { instance={"scratch"}, class={"scratch"} },
    properties = {
      --floating = true,
      --titlebars_enabled = true,
      --minimized = true,
      --sticky = false,
      above = true,
      ontop = false,
      --border_width = 0,
      --skip_taskbar = true,
      --honor_padding = true,
      --honor_workarea = true
  }
}
end)

-- }}}

-- Titlebars {{{
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    awful.button({}, 1,
      function() c:activate { context = "titlebar", action = "mouse_move" } end),
    awful.button({}, 3,
      function() c:activate { context = "titlebar", action = "mouse_resize" } end),
  }

  awful.titlebar(c).widget = {
    {
      awful.titlebar.widget.floatingbutton(c),
      awful.titlebar.widget.stickybutton(c),
      awful.titlebar.widget.ontopbutton(c),
      layout  = wibox.layout.fixed.horizontal
    },
    {
      {
        align  = "center",
        awful.titlebar.widget.iconwidget(c),
        awful.titlebar.widget.titlewidget(c),
        layout  = wibox.layout.flex.horizontal
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    {
      awful.titlebar.widget.minimizebutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.closebutton(c),
      layout = wibox.layout.fixed.horizontal
    },
    layout = wibox.layout.align.horizontal
  }
end)

-- {{{ Notifications

ruled.notification.connect_signal('request::rules', function()
  -- All notifications will match this rule.
  ruled.notification.append_rule {
    rule       = {},
    properties = {
      screen           = awful.screen.preferred,
      implicit_timeout = 5,
    }
  }
end)

naughty.connect_signal("request::display", function(n)
  naughty.layout.box { notification = n }
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
-- awful.tag.history.restore = function() end
--]]

-- {{{ spawn
-- spawn_once = function(program, arguments, process)
--   process = process or program
--   awful.spawn.easy_async("pgrep "..process, function(stdout, stderr, reason, exit_code)
--     naughty.notify { text=exit_code }
--     if exit_code ~= 0 then
--       awful.spawn(program .. " " .. arguments)
--     end
--   end)
-- end
--
-- local startup = {
--   always = {
--     {"primenote", "--core show", "pnote"}
--   },
--   once = {
--   }
-- }
-- for _, cmd in pairs(startup["always"]) do
--   spawn_once(cmd[1], cmd[2] or nil, cmd[3] or nil)
-- end
--for _, cmd in pairs(startup["once"]) do
-- raise_or_spawn, once, single_instance
--end
-- }}}
