-- Imports {{{
import XMonad
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

-- Data {{{
import qualified Data.Map as M
-- }}}

-- Actions {{{
import XMonad.Actions.Promote
-- import XMonad.Actions.MouseResize
--- }}}

-- Hooks {{{
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
-- }}}

-- Util {{{
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.NamedScratchpad
-- }}}

-- Layout {{{
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.Magnifier as Magn
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.Fullscreen
-- import XMonad.Layout.WindowArranger
-- import XMonad.Layout.BorderResize
import XMonad.Layout.ResizableTile -- for resizeable tall layout
import XMonad.Layout.MouseResizableTile -- for mouse control
import XMonad.Layout.Grid          -- for additional grid layout
import XMonad.Layout.NoBorders     -- for fullscreen without borders
-- }}}
-- }}}

main :: IO () -- {{{
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig
-- }}}

myConfig = def -- {{{
    { modMask            = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook         = myLayoutHook  -- Use custom layouts
    , manageHook         = myManageHook  -- Match on certain windows
    , terminal           = "${TERMINAL:-alacritty}"   -- TODO Use ENV
    , focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 3
    , workspaces         = ["1","2","3","4","5","6","7","8","9", "10"]
    , normalBorderColor  = "#383a4a"
    , focusedBorderColor = "#306998"
    , keys               = myKeys
    -- , mouseBindings      = 
    -- , handleEventHook    = 
    -- , startupHook        = 
    }
-- }}}

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $ -- {{{
  [ ((modm                  , xK_Return              ), spawn $ XMonad.terminal conf)
  , ((modm                  , xK_n                   ), namedScratchpadAction myScratchpads "terminal")
  , ((modm     .|. shiftMask, xK_r                   ), spawn "xmonad --recompile && xmonad --restart")
  , ((modm     .|. shiftMask, xK_Return              ), promote)
  , ((modm                  , xK_q                   ), kill)
  , ((modm                  , xK_d                   ), spawn "rofi -show")
  , ((modm     .|. shiftMask, xK_d                   ), spawn "dmenu_run")
  , ((modm     .|. shiftMask, xK_c                   ), spawn "toggleprogram picom -fcCGb --xrender-sync-fence")
  , ((modm                  , xK_x                   ), spawn "betterlockscreen --lock dimblur --blur 8")
  , ((mod1Mask              , xK_p                   ), spawn "screenshot -m region -t -c -o 'screenshot-xbackbone'")
  , ((mod1Mask .|. shiftMask, xK_p                   ), spawn "screenshot -m region -c")
  , ((modm     .|. shiftMask, xK_space               ), setLayout $ XMonad.layoutHook conf)
  , ((modm                  , xK_space               ), sendMessage NextLayout)
  , ((modm                  , xK_f                   ), sequence_ [sendMessage $ Toggle FULL, sendMessage ToggleStruts])
  , ((modm                  , xK_b                   ), sendMessage ToggleStruts)
  , ((modm                  , xK_j                   ), windows W.focusDown)
  , ((modm                  , xK_k                   ), windows W.focusUp)
  , ((modm                  , xK_m                   ), windows W.focusMaster)
  , ((modm     .|. shiftMask, xK_j                   ), windows W.swapDown)
  , ((modm     .|. shiftMask, xK_k                   ), windows W.swapUp)
  , ((modm                  , xK_h                   ), sendMessage Shrink)
  , ((modm                  , xK_l                   ), sendMessage Expand)
  , ((modm                  , xK_comma               ), sendMessage (IncMasterN 1))
  , ((modm                  , xK_period              ), sendMessage (IncMasterN (-1)))
  , ((modm                  , xK_b                   ), sendMessage ToggleStruts)
  , ((modm                  , xK_t                   ), withFocused $ windows . W.sink)
  , ((modm                  , xK_Right               ), sendMessage $ WN.Go R)
  , ((modm                  , xK_Left                ), sendMessage $ WN.Go L)
  , ((modm                  , xK_Up                  ), sendMessage $ WN.Go U)
  , ((modm                  , xK_Down                ), sendMessage $ WN.Go D)
  , ((modm     .|. shiftMask, xK_Right               ), sendMessage $ WN.Swap R)
  , ((modm     .|. shiftMask, xK_Left                ), sendMessage $ WN.Swap L)
  , ((modm     .|. shiftMask, xK_Up                  ), sendMessage $ WN.Swap U)
  , ((modm     .|. shiftMask, xK_Down                ), sendMessage $ WN.Swap D)
  , ((modm     .|. shiftMask .|. mod1Mask, xK_q      ), io (exitWith ExitSuccess))
  , ((0                     , xF86XK_AudioPlay       ), spawn "playerctl play-pause")
  , ((0                     , xF86XK_AudioPause      ), spawn "playerctl play-pause")
  , ((0                     , xF86XK_AudioNext       ), spawn "playerctl next")
  , ((0                     , xF86XK_AudioPrev       ), spawn "playerctl previous")
  , ((0                     , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 2 -5%")
  , ((0                     , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 2 +5%")
  ] ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [ ((m        .|. modm     , k                      ), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [ ((m        .|. modm     , key                    ), screenWorkspace sc >>= flip whenJust (windows . f))
  --   | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  --   , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
  -- ]
-- }}}

myScratchpads :: [NamedScratchpad] -- {{{
myScratchpads = [NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm  = "${TERMINAL:-alacritty}" ++ " --class scratchpad --title scratchpad --option font.size=14 --command tmux new-session -A -s scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
-- }}}

myManageHook :: ManageHook -- {{{
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Sxiv"           --> doFloat
    , className =? "Pavucontrol"    --> doCenterFloat
    , title     =? "scratchpad"     --> doCenterFloat
    , className =? "discord"        --> doShift "2" -- TODO Use second item in workspaces
    , className =? "Steam"          --> doShift "5"
    , className =? "Remmina"        --> doShift "5"
    , className =? "Spotify"        --> doShift "8"
    , resource  =? "desktop_window" --> doIgnore
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
    , isDialog                      --> doFloat
    ] <+> namedScratchpadManageHook myScratchpads
-- }}}

myLayoutHook -- {{{
  = smartBorders
  . spacing
  . avoidStruts
  . WN.windowNavigation
  . mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ reflectHoriz (reflectVert emptyBSP)
  ||| tiled
  ||| threeCol
  ||| Full
  where
    spacing   = spacingRaw True (Border 0 10 10 10) True (Border 5 5 5 5) True -- spacingWithEdge 5
    threeCol  = Magn.magnifiercz' magnifier $ ThreeColMid nmaster delta ratio
    magnifier = 1.35   -- Amount to zoom the windows by
    tiled     = Tall nmaster delta ratio
    nmaster   = 1      -- Default number of windows in the master pane
    ratio     = 1/2    -- Default proportion of screen occupied by master pane
    delta     = 3/100  -- Percent of screen to increment by when resizing panes
-- }}}

myXmobarPP :: PP -- {{{
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
-- }}}
