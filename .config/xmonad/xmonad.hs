-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs

-- Imports {{{
import XMonad
import XMonad.Util.Dmenu
import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

-- System
import System.Exit

-- Data
import Data.List as L
import qualified Data.Map as M

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.FlexibleResize as Flex

-- Hooks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing
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
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.Magnifier as Magn
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
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

myEventHook = swallowEventHook (className =? "Alacritty") (return True)

myConfig = def -- {{{
    { modMask            = mod4Mask::KeyMask
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , terminal           = "${TERMINAL:-alacritty}"::String -- TODO System.Environment
    , focusFollowsMouse  = True::Bool
    , clickJustFocuses   = True::Bool
    , borderWidth        = 2::Dimension
    , workspaces         = map show $ [1..9::Int]++[0::Int]::[WorkspaceId]
    , normalBorderColor  = "#383a4a"::String
    , focusedBorderColor = "#306998"::String
    , keys               = myKeyMaps
    , mouseBindings      = myButtons
    , startupHook        = myStartupHook
    , handleEventHook    = myEventHook
    } `additionalKeysP` myKeys
-- }}}

confirm :: String -> X () -> X () -- {{{
confirm msg cb = do
    res <- dmenu ["", msg]
    when (res == msg) cb
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
-- }}}

-- Get the name of the active layout.
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
    workspaces <- gets windowset
    return $ description . W.layout . W.workspace . W.current $ workspaces

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
  , ("M-p", namedScratchpadAction myScratchpads "calculator")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-<Return>", promote)
  , ("M-S-f", floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat'))
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
  , ("M-S-b", sendMessage ToggleStruts)
  , ("M-m", sendMessage $ IncMasterN $ 1)
  , ("M-S-m", sendMessage $ IncMasterN $ -1)
  , ("M-t", withFocused $ windows . W.sink)
  -- https://stackoverflow.com/questions/7603509/haskell-syntax-for-or-in-case-expressions
  , ("M-<Right>", sendMessage $ WN.Go R)
  , ("M-<Left>", sendMessage $ WN.Go L)
  , ("M-<Up>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Full","Full"] -> windows W.focusUp
      _                                  -> sendMessage $ WN.Go U)
  , ("M-<Down>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Full","Full"] -> windows W.focusDown
      _                                  -> sendMessage $ WN.Go D)
  , ("M-S-<Right>", floatOrNot (withFocused (keysMoveWindow ( 20,  0))) (sendMessage $ WN.Swap R))
  , ("M-S-<Left>",  floatOrNot (withFocused (keysMoveWindow (-20,  0))) (sendMessage $ WN.Swap L))
  , ("M-S-<Up>",    floatOrNot (withFocused (keysMoveWindow (  0,-20))) (sendMessage $ WN.Swap U))
  , ("M-S-<Down>",  floatOrNot (withFocused (keysMoveWindow (  0, 20))) (sendMessage $ WN.Swap D))
  , ("M-C-<Right>", floatOrNot (withFocused (keysResizeWindow ( 20,   0) (0, 0))) (sendMessage Expand))
  , ("M-C-<Left>",  floatOrNot (withFocused (keysResizeWindow (-20,   0) (0, 0))) (sendMessage Shrink))
  , ("M-C-<Up>",    floatOrNot (withFocused (keysResizeWindow (  0, -20) (0, 0))) (sendMessage Expand))
  , ("M-C-<Down>",  floatOrNot (withFocused (keysResizeWindow (  0,  20) (0, 0))) (sendMessage Shrink))
  , ("M-S-e", confirm "Exit" $ io (exitWith ExitSuccess))
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
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = "${TERMINAL:-alacritty}" ++ " --class scratchpad --title scratchpad --option font.size=12 --command tmux new-session -A -s scratchpad"
    findTerm   = appName =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
           where
             h = 0.9
             w = 0.9
             t = 0.95 -h
             l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
           where
             h = 0.5
             w = 0.4
             t = 0.75 -h
             l = 0.70 -w
-- }}}

myManageHook :: ManageHook -- {{{
myManageHook = insertPosition Below Newer <>
    let w = workspaces myConfig in composeAll
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

-- . mkToggle (NBFULL ?? NOBORDERS ?? EOT)

masterStack = renamed [Replace "Tiled"]
            $ ResizableTall 1 (3/100) (1/2) []
bsp         = renamed [Replace "BSP"]
            $ emptyBSP
threeCol    = renamed [Replace "ThreeCol"]
            $ Magn.magnifiercz' 1.35
            $ ThreeColMid 1 (3/100) (1/2)

myLayoutHook
  = avoidStruts
  . mouseResize
  . windowArrange
  . smartBorders
  . spacingRaw True (Border 0 10 10 10) True (Border 5 5 5 5) True
  . WN.windowNavigation
  $ myLayouts
  where
    myLayouts = masterStack
            ||| threeCol
            ||| bsp

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
