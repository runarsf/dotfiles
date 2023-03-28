-- TODO https://www.reddit.com/r/xmonad/comments/11qrizh/how_to_show_my_current_layout_in_tint2
-- TODO https://github.com/dylanaraps/pywal/issues/172

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
import Data.Ratio
import Data.Word (Word32)
import qualified Data.Map as M

-- Actions
import XMonad.Actions.Warp
import XMonad.Actions.Promote
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS as CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.ShowText
import XMonad.Actions.FloatSnap
import XMonad.Actions.EasyMotion (selectWindow)
import qualified XMonad.Actions.FlexibleResize as Flex
-- import XMonad.Actions.TopicSpace

-- Hooks
import XMonad.Hooks.ServerMode
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.Place
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Hooks.BorderPerWindow (defineBorderWidth, actionQueue)
import XMonad.Hooks.SetWMName (setWMName)
-- import XMonad.Hooks.UrgencyHook

-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.NamedScratchpad

-- Layout
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.Fullscreen
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.WindowNavigation as WN
import qualified XMonad.Layout.Magnifier as Magn
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.Combo
-- import XMonad.Layout.BorderResize
-- }}}

main :: IO () -- {{{
main = xmonad
     $ docks
     . ewmhFullscreen
     . ewmh
     . pagerHints
     $ myConfig
-- }}}

myConfig = def -- {{{
    { modMask            = mod4Mask::KeyMask
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , terminal           = "${TERMINAL:-alacritty}"::String -- TODO System.Environment https://stackoverflow.com/a/60715978
    , focusFollowsMouse  = True::Bool
    , clickJustFocuses   = False::Bool
    , borderWidth        = 2::Dimension
    , workspaces         = map show $ [1..9::Int]++[0::Int]::[WorkspaceId]
    , normalBorderColor  = "#383a4a"::String
    , focusedBorderColor = "#306998"::String
    , mouseBindings      = myButtons
    , startupHook        = myStartupHook
    , handleEventHook    = myEventHook
    } `additionalKeysP` myKeys
-- }}}

-- TODO Use rofi for prompts
confirm :: String -> X () -> X () -- {{{
confirm msg cb = do
    res <- dmenu ["", msg]
    when (res == msg) cb
-- }}}

-- Resizing tray automatically: https://www.reddit.com/r/xmonad/comments/10qcqcr/comment/j6q6m31/
myStartupHook :: X () -- {{{
myStartupHook = do
  spawn "(pgrep eww && eww reload) || (eww close bar || killall -q eww; eww open bar)"
  spawn "killall -q trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --widthtype request --transparent true --alpha 0 --tint 0x0D1117 --height 30 --heighttype pixel --monitor 'primary' --margin 20 --distance 11 --padding 0 &"
  spawn "(nitrogen --restore || (~/.fehbg || feh --bg-scale ~/.config/wall.jpg)) &"
  -- spawn "killall -q picom; picom -fcCGb --xrender-sync-fence &"
  setWMName "LG3D"
-- }}}

-- float toggle {{{

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
centreFloat' w = windows $ W.float w (W.RationalRect 0.25 0.25 0.5 0.5)

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
  [ ((modm,               button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 25) (Just 25) w)))
  , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize [L,R,U,D] (Just 500) (Just 500) w)))
  , ((modm,               button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R,D] (Just 25) (Just 25) w)))
  ]
-- }}}

myKeys :: [([Char], X ())] -- {{{
myKeys =
  [ ("M-<Return>", spawn $ terminal myConfig)
  , ("M-n", do
    namedScratchpadAction myScratchpads "terminal"
    mouseFollowFocus)
  , ("M-p", do
    namedScratchpadAction myScratchpads "calculator"
    mouseFollowFocus)
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-<Return>", promote)
  , ("M-S-f", floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat'))
  , ("M-q", kill)
  , ("M-S-q", spawn "xkill")
  , ("M-a", windows copyToAll)
  , ("M-S-a",  killAllOtherCopies)
  , ("M-d", spawn "rofi -show")
  , ("M-S-d", spawn "dmenu_run")
  , ("M-e", spawn "xdg-open ~")
  , ("M-S-c", spawn "toggleprogram picom -fcCGb --xrender-sync-fence")
  , ("M-x", spawn "betterlockscreen --lock dimblur --blur 8")
  , ("M-s", selectWindow def >>= (`whenJust` windows . W.focusWindow))
  , ("M1-p", spawn "screenshot -m region -t -c -o 'screenshot-xbackbone'")
  , ("M1-S-p", spawn "screenshot -m region -c")
  , ("M-<Tab>", toggleRecentWS)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", sendMessage FirstLayout)
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  -- , ("M-f", sequence_ [sendMessage $ Toggle FULL, sendMessage ToggleStruts])
  , ("M-f", toggleFull)
  , ("M-M1-b", sendMessage ToggleStruts)
  , ("M-M1-<Down>", sendMessage $ weakModifyGaps decGaps)
  , ("M-M1-<Up>", sendMessage $ weakModifyGaps incGaps)
  , ("M-m", sendMessage $ IncMasterN $ 1)
  , ("M-S-m", sendMessage $ IncMasterN $ -1)
  , ("M-S-o", do
    layout <- getActiveLayoutDescription
    flashText def 1 layout)
  , ("M-o", swapGaps)
  , ("M-<Left>", do
    sendMessage $ WN.Go L
    mouseFollowFocus)
  , ("M-<Right>", do
    sendMessage $ WN.Go R
    mouseFollowFocus)
  , ("M-<Up>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Monocle","Spacing Dual"] -> windows W.focusUp
      _                                             -> sendMessage $ WN.Go U
    mouseFollowFocus)
  , ("M-<Down>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Monocle","Spacing Dual"] -> windows W.focusDown
      _                                  -> sendMessage $ WN.Go D
    mouseFollowFocus)
  , ("M-S-<Right>", floatOrNot (withFocused (keysMoveWindow ( 20,  0))) (sendMessage $ WN.Swap R))
  , ("M-S-<Left>",  floatOrNot (withFocused (keysMoveWindow (-20,  0))) (sendMessage $ WN.Swap L))
  , ("M-S-<Up>",    floatOrNot (withFocused (keysMoveWindow (  0,-20))) (sendMessage $ WN.Swap U))
  , ("M-S-<Down>",  floatOrNot (withFocused (keysMoveWindow (  0, 20))) (sendMessage $ WN.Swap D))
  , ("M-C-<Right>", floatOrNot (withFocused (keysResizeWindow ( 20,   0) (0, 0))) (sendMessage Expand))
  , ("M-C-<Left>",  floatOrNot (withFocused (keysResizeWindow (-20,   0) (0, 0))) (sendMessage Shrink))
  , ("M-C-<Up>",    floatOrNot (withFocused (keysResizeWindow (  0, -20) (0, 0))) (sendMessage MirrorExpand))
  , ("M-C-<Down>",  floatOrNot (withFocused (keysResizeWindow (  0,  20) (0, 0))) (sendMessage MirrorShrink))
  , ("M-S-e", confirm "Exit" $ io (exitWith ExitSuccess))
  , ("<XF86AudioPause>",       spawn "playerctl play-pause")
  , ("<XF86AudioPlay>",        spawn "playerctl play-pause")
  , ("<XF86AudioNext>",        spawn "playerctl next")
  , ("<XF86AudioPrev>",        spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  ] ++
  [(mask ++ "M-" ++ key, action tag)
        | (tag, key) <- zip (workspaces myConfig) (map show $ [1..9::Int]++[0::Int])
        , (mask, action) <- [ ("", windows . W.greedyView), ("S-", windows . W.shift) ]
  ] ++
  [(mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
        | (key, scr) <- zip "tyu" [2,0,1]
        , (action, mask) <- [(W.view, ""), (W.shift, "S-")]]
  where
      decGaps d i | d `elem` [L, R, U, D] = i - 10
      incGaps d i | d `elem` [L, R, U, D] = i + 10
      mouseFollowFocus = do
        warpToWindow (1%2) (1%2)
        banish UpperRight
-- }}}

myScratchpads :: [NamedScratchpad] -- {{{
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = "${TERMINAL:-alacritty}" ++ " --class scratchpad --title scratchpad --option font.size=12 --command tmux new-session -A -s scratchpad"
    findTerm   = appName =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect x y w h
           where
             x = 0.2
             y = (1/6)
             w = 0.6
             h = (2/3)
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect x y w h
           where
             x = 0.3
             y = (1/6)
             w = 0.4
             h = (2/3)
-- }}}

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c

-- FIXME Breaks things like `doPipFloat`
myPlaceHook :: Placement
myPlaceHook =
  inBounds
  $ withGaps (defaultGapW, defaultGapW, defaultGapW, defaultGapW) (smart (1%2, 1%2))

-- TODO Copy all PiP windows to all screens
-- TODO https://stackoverflow.com/a/74252752
-- TODO https://github.com/xmonad/xmonad/issues/152
-- NOTE https://gist.github.com/tylevad/3146111
role = stringProperty "WM_WINDOW_ROLE"
-- wtype = stringProperty "_NET_WM_WINDOW_TYPE"
-- wtype =? "_NET_WM_WINDOW_TYPE_UTILITY" --> dothisthing
-- , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" --> defineBorderWidth 0

myManageHook :: ManageHook -- {{{
myManageHook =
    insertPosition Below Newer
    <+> namedScratchpadManageHook myScratchpads
    <> let w = workspaces myConfig in composeAll
    [ fmap not willFloat                      --> insertPosition Below Newer
    , fmap not isDialog                       --> doF avoidMaster
    , isFullscreen                            --> doFullFloat
    , isDialog                                --> doCenterFloat
    , isDialog                                --> doF W.shiftMaster <+> doF W.swapDown
    , appName   =? "panel"                    --> doLower
    , resource  =? "desktop_window"           --> doIgnore
    , role      ~? "PictureInPicture"         --> doPipFloat
    , className ~? "eww-"                     --> doLower
    , className =? "PrimeNote"                --> doFloat
    , className =? "Gimp"                     --> doCenterFloat
    , className =? "Sxiv"                     --> doCenterFloat
    , appName   =? "scratchpad"               --> doCenterFloat
    , className =? "Dragon-drop"              --> doCenterFloat
    , className =? "Blueman-manager"          --> doCenterFloat
    , className =? "ColorGrab"                --> doCenterFloat
    ,(className =? "discord"                  <&&>
      fmap (not . (" - Discord" `isSuffixOf`)) title ) --> doPipFloat
    ,(className ~? "firefox"                  <&&>
      resource  =? "Dialog")                  --> doCenterFloat
    , className =? "discord"                  --> doShift (w !! 1)
    , className =? "Steam"                    --> doShift (w !! 3)
    , className =? "spotify"                  --> doShift (w !! 3)
    , className =? "VirtualBox Manager"       --> doShift (w !! 4)
    , className =? "org.remmina.Remmina"      --> doShift (w !! 4)
    , title     =? "AudioRelay"               --> doShift (w !! 6)
    , className =? "Pavucontrol"              --> doShift (w !! 6)
    , className =? "easyeffects"              --> doShift (w !! 6)
    , className =? "Blueman-manager"          --> doShift (w !! 6)
    , className =? "Carla2"                   --> doShift (w !! 6)
    , className =? "helvum"                   --> doShift (w !! 6)
    ]
    <+> placeHook myPlaceHook
-- }}}

doPipFloat = (customFloating $ W.RationalRect x y w h)
  where
    w = (1/4)
    h = (1/4)
    x = 1 - w - 0.005
    y = 1 - h - 0.005

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

myEventHook =
  handleTimerEvent
  <+> swallowEventHook (className =? "Alacritty") (return True)

-- Layouts {{{
masterStack = renamed [Replace "Tiled"]
            $ ResizableTall 1 (3/100) (1/2) []
bsp         = renamed [Replace "BSP"]
            $ emptyBSP
threeCol    = renamed [Replace "ThreeCol"]
            $ Magn.magnifiercz' 1.35
            $ ResizableThreeColMid 1 (3/100) (1/2) []
dual        = renamed [Replace "Dual"]
            $ TwoPanePersistent Nothing (3/100) (1/2)
monocle     = renamed [Replace "Monocle"]
            $ noBorders
            $ Full
-- }}}

-- https://www.reddit.com/r/xmonad/comments/ygp2ab/toggle_between_two_sets_of_gaps/
defaultGap :: Int
defaultGap = 10
defaultGapW :: Word32
defaultGapW = fromIntegral defaultGap
defaultGaps = [(U,defaultGap), (R,defaultGap), (D,defaultGap), (L,defaultGap)]
swapGaps = sendMessage . ModifyGaps $ \gs ->
       if gs == a then b
                  else a
  where
    a = defaultGaps
    b = [(U,10),(R,75),(D,75),(L,75)]
    -- c = [(U,0),(R,0),(D,0),(L,0)]

myLayoutHook -- {{{
  = avoidStruts
  . mouseResize
  . windowArrange
  . smartBorders
  . WN.windowNavigation
  . mkToggle (NBFULL ?? NOBORDERS ?? EOT) -- (NBFULL ?? NOBORDERS ?? EOT
  . spacingRaw True (Border 0 0 0 0) True (Border 10 10 10 10) True -- Spacing between windows
  . gaps defaultGaps -- Gaps between screen and windows
  $ myLayouts
  where
    myLayouts = onWorkspaces ["5"] monocle masterStack
            ||| onWorkspaces ["2"] threeCol dual
            ||| onWorkspaces ["2"] dual threeCol
            ||| bsp
            ||| onWorkspaces ["5"] masterStack monocle
-- }}}
