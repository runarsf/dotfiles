-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs

-- Imports {{{
import XMonad
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

-- System
import System.Exit (exitSuccess)

-- Data
import Data.List as L
import qualified Data.Map as M

-- Actions
import XMonad.Actions.Promote
-- import XMonad.Actions.MouseResize
import qualified XMonad.Actions.FlexibleResize as Flex

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName (setWMName)

-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.NamedScratchpad

-- Layout
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.Fullscreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.Magnifier as Magn
-- import XMonad.Layout.WindowArranger
-- import XMonad.Layout.BorderResize
-- }}}

main :: IO () -- {{{
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     $ myConfig
     -- . withEasySB (statusBarProp "xmobar ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
-- }}}

myConfig = def -- {{{
    { modMask            = mod4Mask::KeyMask
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , terminal           = "${TERMINAL:-alacritty}"::String -- TODO System.Environment
    , focusFollowsMouse  = True::Bool
    , clickJustFocuses   = True::Bool
    , borderWidth        = 1::Dimension
    , workspaces         = map show $ [1..9::Int]++[0::Int]::[WorkspaceId]
    , normalBorderColor  = "#383a4a"::String
    , focusedBorderColor = "#306998"::String
    , keys               = myKeyMaps
    , mouseBindings      = myButtons
    , startupHook        = myStartupHook
    -- , handleEventHook    = 
    } `additionalKeysP` myKeys
-- }}}

myStartupHook :: X () -- {{{
myStartupHook = do
  spawn "(pgrep eww && eww reload) || (eww close bar || killall -q eww; eww open bar)"
  spawn "killall -q trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --widthtype pixel --width 125 --transparent true --alpha 0 --tint 0x0D1117 --height 30 --heighttype pixel --monitor 'primary' --margin 20 --distance 11 --padding 0 &"
  setWMName "LG3D"
-- }}}

-- float toggle {{{
centreRect = W.RationalRect 0.25 0.25 0.5 0.5

-- If the window is floating then (f), if tiled then (n)
floatOrNot f n = withFocused $ \windowId -> do
    floats <- gets (W.floating . windowset)
    if windowId `M.member` floats -- if the current window is floating...
       then f
       else n

-- Centre and float a window (retain size)
centreFloat win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
    return ()

-- Float a window in the centre
centreFloat' w = windows $ W.float w centreRect

-- Make a window my 'standard size' (half of the screen) keeping the centre of the window fixed
standardSize win = do
    (_, W.RationalRect x y w h) <- floatLocation win
    windows $ W.float win (W.RationalRect x y 0.5 0.5)
    return ()


-- Float and centre a tiled window, sink a floating window
toggleFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat')
-- }}}

myButtons conf@XConfig {XMonad.modMask = modm} = M.fromList $ -- {{{
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                    >> windows W.shiftMaster)
  , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w
                                    >> windows W.shiftMaster)
  ]
-- }}}

myKeys :: [([Char], X ())] -- {{{
myKeys =
  [ ("M-<Return>", spawn $ terminal myConfig) -- XMonad.terminal conf
  , ("M-n", namedScratchpadAction myScratchpads "terminal")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-<Return>", promote)
  , ("M-S-f", toggleFloat)
  , ("M-q", kill)
  , ("M-d", spawn "rofi -show")
  , ("M-S-d", spawn "dmenu_run")
  , ("M-S-c", spawn "toggleprogram picom -fcCGb --xrender-sync-fence")
  , ("M-x", spawn "betterlockscreen --lock dimblur --blur 8")
  , ("M1-p", spawn "screenshot -m region -t -c -o 'screenshot-xbackbone'")
  , ("M1-S-p", spawn "screenshot -m region -c")
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", sendMessage FirstLayout)
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("M-f", sequence_ [sendMessage $ Toggle FULL, sendMessage ToggleStruts])
  , ("M-b", sendMessage ToggleStruts)
  -- , ("M-j", windows W.focusDown)
  -- , ("M-k", windows W.focusUp)
  -- , ("M-S-j", windows W.swapDown)
  -- , ("M-S-k", windows W.swapUp)
  -- , ("M-h", sendMessage Shrink)
  -- , ("M-l", sendMessage Expand)
  , ("M-m", sendMessage $ IncMasterN $ 1)
  , ("M-S-m", sendMessage $ IncMasterN $ -1)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-<Right>", sendMessage $ WN.Go R)
  , ("M-<Left>",  sendMessage $ WN.Go L)
  , ("M-<Up>",    sendMessage $ WN.Go U)
  , ("M-<Down>",  sendMessage $ WN.Go D)
  , ("M-S-<Right>", sendMessage $ WN.Swap R)
  , ("M-S-<Left>",  sendMessage $ WN.Swap L)
  , ("M-S-<Up>",    sendMessage $ WN.Swap U)
  , ("M-S-<Down>",  sendMessage $ WN.Swap D)
  , ("M-C-<Right>", sendMessage Expand)
  , ("M-C-<Left>",  sendMessage Shrink)
  , ("M-C-<Up>",    sendMessage Expand)
  , ("M-C-<Down>",  sendMessage Shrink)
  , ("M-S-e", io exitSuccess)
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 2 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 2 +5%")
  , ("<XF86AudioPause>", spawn "playerctl play-pause") ]
-- }}}

myKeyMaps conf@XConfig {XMonad.modMask = modm} = M.fromList $ -- {{{
  [ ((m        .|. modm     , k                      ), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [ ((m        .|. modm     , key                    ), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
  ]
-- }}}

myScratchpads :: [NamedScratchpad] -- {{{
myScratchpads = [NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm  = "${TERMINAL:-alacritty}" ++ " --class scratchpad --title scratchpad --option font.size=12 --command tmux new-session -A -s scratchpad"
    findTerm   = appName =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w
-- }}}

myManageHook :: ManageHook -- {{{
myManageHook = let w = workspaces myConfig in composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Sxiv"           --> doFloat
    , className =? "Pavucontrol"    --> doCenterFloat
    , title     =? "scratchpad"     --> doCenterFloat
    , className =? "discord"        --> doShift (w !! 1)
    , className =? "Steam"          --> doShift (w !! 4)
    , className =? "Remmina"        --> doShift (w !! 5)
    , className =? "Spotify"        --> doShift (w !! 7)
    , className ~? "eww-"           --> doLower
    , resource  =? "desktop_window" --> doIgnore
    ,(className =? "firefox"        <&&>
      resource  =? "Dialog")        --> doFloat
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
