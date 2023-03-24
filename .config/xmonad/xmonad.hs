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
import qualified Data.Map as M

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.ShowText
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
     $ myConfig
     -- . withEasySB (statusBarProp "xmobar ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
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
    , keys               = myKeyMaps
    , mouseBindings      = myButtons
    , startupHook        = myStartupHook
    , handleEventHook    = handleTimerEvent
    -- , handleEventHook    = myServerModeEventHook
    -- , handleEventHook    = serverModeEventHookCmd
    --                          <+> serverModeEventHook
    --                          <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
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
  setWMName "LG3D"
  -- spawn "killall -q picom; picom -fcCGb --xrender-sync-fence &"
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

kill8 ss | Just w <- W.peek ss = (W.insertUp w) $ W.delete w ss
         | otherwise = ss

myKeys :: [([Char], X ())] -- {{{
myKeys =
  [ ("M-<Return>", spawn $ terminal myConfig)
  , ("M-n", namedScratchpadAction myScratchpads "terminal")
  , ("M-p", namedScratchpadAction myScratchpads "calculator")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-<Return>", promote)
  , ("M-S-f", floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat'))
  , ("M-q", kill)
  , ("M-a", sequence_ $ [windows $ copy i | i <- XMonad.workspaces myConfig])
  , ("M-S-a", windows $ kill8) -- FIXME don't un-float when un-pinning
  , ("M-d", spawn "rofi -show")
  , ("M-S-d", spawn "dmenu_run")
  , ("M-e", spawn "xdg-open ~")
  , ("M-S-c", spawn "toggleprogram picom -fcCGb --xrender-sync-fence")
  , ("M-x", spawn "betterlockscreen --lock dimblur --blur 8")
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
  , ("M-t", withFocused $ windows . W.sink)
  -- https://stackoverflow.com/questions/7603509/haskell-syntax-for-or-in-case-expressions
  , ("M-<Right>", sendMessage $ WN.Go R)
  , ("M-<Left>", sendMessage $ WN.Go L)
  , ("M-o", do
    layout <- getActiveLayoutDescription
    flashText def 1 layout)
  , ("M-S-o", swapGaps)
  , ("M-<Up>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Monocle","Spacing Dual"] -> windows W.focusUp
      _                                  -> sendMessage $ WN.Go U)
  , ("M-<Down>", do
    layout <- getActiveLayoutDescription
    case layout of
      x | elem x ["Spacing Monocle","Spacing Dual"] -> windows W.focusDown
      _                                  -> sendMessage $ WN.Go D)
  , ("M-S-<Right>", floatOrNot (withFocused (keysMoveWindow ( 20,  0))) (sendMessage $ WN.Swap R))
  , ("M-S-<Left>",  floatOrNot (withFocused (keysMoveWindow (-20,  0))) (sendMessage $ WN.Swap L))
  , ("M-S-<Up>",    floatOrNot (withFocused (keysMoveWindow (  0,-20))) (sendMessage $ WN.Swap U))
  , ("M-S-<Down>",  floatOrNot (withFocused (keysMoveWindow (  0, 20))) (sendMessage $ WN.Swap D))
  , ("M-C-<Right>", floatOrNot (withFocused (keysResizeWindow ( 20,   0) (0, 0))) (sendMessage Expand))
  , ("M-C-<Left>",  floatOrNot (withFocused (keysResizeWindow (-20,   0) (0, 0))) (sendMessage Shrink))
  , ("M-C-<Up>",    floatOrNot (withFocused (keysResizeWindow (  0, -20) (0, 0))) (sendMessage MirrorExpand))
  , ("M-C-<Down>",  floatOrNot (withFocused (keysResizeWindow (  0,  20) (0, 0))) (sendMessage MirrorShrink))
  , ("M-S-e", confirm "Exit" $ io (exitWith ExitSuccess))
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 2 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 2 +5%")
  , ("<XF86AudioPause>", spawn "playerctl play-pause") ]
  where
      decGaps d i | d `elem` [L, R, U, D] = i - 10
      incGaps d i | d `elem` [L, R, U, D] = i + 10
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

-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c

-- myPlaceHook :: Placement
-- myPlaceHook = inBounds $ smart(1, 1)

myManageHook :: ManageHook -- {{{
myManageHook = (isDialog --> doF W.shiftMaster <+> doF W.swapDown)
    <+> (fmap not isDialog --> doF avoidMaster)
    <+> insertPosition Below Newer
    -- <+> placeHook myPlaceHook
    <+> namedScratchpadManageHook myScratchpads
    <>  let w = workspaces myConfig in composeAll
    [ className =? "Gimp"                     --> doCenterFloat
    , className =? "Sxiv"                     --> doCenterFloat
    , appName   =? "scratchpad"               --> doCenterFloat
    , className =? "Dragon-drop"              --> doCenterFloat
    , className =? "Pavucontrol"              --> doShift (w !! 6)
    , className =? "easyeffects"              --> doShift (w !! 6)
    , className =? "discord"                  --> doShift (w !! 1)
    , className =? "VirtualBox Manager"       --> doShift (w !! 4)
    , className =? "org.remmina.Remmina"      --> doShift (w !! 4)
    , className =? "Steam"                    --> doShift (w !! 3)
    , className =? "spotify"                  --> doShift (w !! 3)
    , className ~? "eww-"                     --> doLower
    , className =? "PrimeNote"                --> doFloat
    , appName   =? "panel"                    --> doLower
    , resource  =? "desktop_window"           --> doIgnore
    ,(className ~? "firefox"                  <&&>
      resource  =? "Dialog")                  --> doCenterFloat
    , isDialog                                --> doCenterFloat
    , fmap not willFloat                      --> insertPosition Below Newer
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" --> defineBorderWidth 0
    ]
-- }}}

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

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
defaultGaps = [(U,10), (R,10), (D,10), (L,10)]
swapGaps = sendMessage . ModifyGaps $ \gs ->
       if gs == a then b
  else if gs == b then c
                  else a
  where
    a = defaultGaps
    b = [(U,10),(R,75),(D,75),(L,75)]
    c = [(U,0),(R,0),(D,0),(L,0)]

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
            ||| dual
            ||| threeCol
            ||| bsp
            ||| onWorkspaces ["5"] masterStack monocle
-- }}}
