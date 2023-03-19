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
import Data.Ratio
import qualified Data.Map as M

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.FloatKeys
import XMonad.Actions.MouseResize
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.FlexibleResize as Flex

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
     $ docks
     . ewmhFullscreen
     . ewmh
     $ myConfig
     -- . withEasySB (statusBarProp "xmobar ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
-- }}}

-- Server mode commands{{{
myCommands :: [(String, X ())]
myCommands =
    [ ("decrease-master-size"   , sendMessage Shrink                                                )
    , ("increase-master-size"   , sendMessage Expand                                                )
    , ("decrease-master-count"  , sendMessage $ IncMasterN (-1)                                     )
    , ("increase-master-count"  , sendMessage $ IncMasterN (1)                                      )
    , ("focus-prev"             , windows W.focusUp                                                 )
    , ("focus-next"             , windows W.focusDown                                               )
    , ("focus-master"           , windows W.focusMaster                                             )
    , ("swap-with-prev"         , windows W.swapUp                                                  )
    , ("swap-with-next"         , windows W.swapDown                                                )
    , ("swap-with-master"       , windows W.swapMaster                                              )
    , ("kill-window"            , kill                                                              )
    , ("quit"                   , io $ exitWith ExitSuccess                                         )
    , ("restart"                , spawn "xmonad --recompile; xmonad --restart"                      )
    , ("change-layout"          , sendMessage NextLayout                                            )
    -- , ("reset-layout"           , setLayout $ XMonad.layoutHook conf                                )
    , ("fullscreen"             , sequence_ [sendMessage $ Toggle FULL, sendMessage ToggleStruts]   )
    ]
-- }}}
-- Server mode event hook {{{
myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
myCommands' = ("list-commands", listMyServerCmds) : myCommands ++ wscs ++ sccs -- ++ spcs
    where
        wscs = [((m ++ s), windows $f s) | s <- (workspaces myConfig)
               , (f, m) <- [(W.view, "focus-workspace-"), (W.shift, "send-to-workspace-")] ]

        sccs = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
               | sc <- [0..3], (f, m) <- [(W.view, "focus-screen-"), (W.shift, "send-to-screen-")]]

--        spcs = [("toggle-" ++ sp, namedScratchpadAction myScratchpads sp)
--               | sp <- (flip map) (myScratchpads) (\(NS x _ _ _) -> x) ]

listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "' | xmessage -file -")
    where asmc = concat $ "Available commands:" : map (\(x, _)-> "    " ++ x) myCommands'
-- }}}

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
    , handleEventHook    = myServerModeEventHook
    -- , handleEventHook    = serverModeEventHookCmd
    --                          <+> serverModeEventHook
    --                          <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
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

kill8 ss | Just w <- W.peek ss = (W.insertUp w) $ W.delete w ss
         | otherwise = ss

myKeys :: [([Char], X ())] -- {{{
myKeys =
  [ ("M-<Return>", spawn $ terminal myConfig) -- XMonad.terminal conf
  , ("M-n", namedScratchpadAction myScratchpads "terminal")
  , ("M-p", namedScratchpadAction myScratchpads "calculator")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-<Return>", promote)
  , ("M-S-f", floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat'))
  , ("M-q", kill)
  , ("M-a", sequence_ $ [windows $ copy i | i <- XMonad.workspaces myConfig])
  , ("M-S-a", windows $ kill8)
  , ("M-d", spawn "rofi -show")
  , ("M-S-d", spawn "dmenu_run")
  , ("M-S-c", spawn "toggleprogram picom -fcCGb --xrender-sync-fence")
  , ("M-x", spawn "betterlockscreen --lock dimblur --blur 8")
  , ("M1-p", spawn "screenshot -m region -t -c -o 'screenshot-xbackbone'")
  , ("M1-S-p", spawn "screenshot -m region -c")
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-<Space>", sendMessage FirstLayout)
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  -- , ("M-f", sequence_ [sendMessage $ Toggle FULL, sendMessage ToggleStruts])
  , ("M-f", toggleFull)
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

                           -- <&&> resource /=? "Dialog"
                           -- <&&> fmap not isDialog -- FIXME Doesn't work, don't swallow dialogs
-- myEventHook = swallowEventHook (className =? "Alacritty"
--                            <||> className =? "org.remmina.Remmina"
--                            -- <||> className ~? "VirtualBox "
--                                ) (return True)

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
    [ className =? "Gimp"                --> doFloat
    , className =? "Sxiv"                --> doFloat
    , className =? "Pavucontrol"         --> doCenterFloat
    , appName   =? "scratchpad"          --> doCenterFloat
    , className =? "discord"             --> doShift (w !! 1)
    , className =? "VirtualBox Manager"  --> doShift (w !! 3)
    , className =? "org.remmina.Remmina" --> doShift (w !! 4)
    , className =? "Steam"               --> doShift (w !! 4)
    , className =? "Spotify"             --> doShift (w !! 7)
    , className ~? "eww-"                --> doLower
    , className =? "PrimeNote"           --> doFloat
    , appName   =? "panel"               --> doLower
    , resource  =? "desktop_window"      --> doIgnore
    ,(className =? "firefox"             <&&>
      resource  =? "Dialog")             --> doFloat
    , isDialog                           --> doFloat
    ]
-- }}}

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })  

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
  . mkToggle (NOBORDERS ?? FULL ?? EOT) -- (NBFULL ?? NOBORDERS ?? EOT)
  $ myLayouts
  where
    myLayouts = masterStack
            ||| threeCol
            ||| bsp
            ||| Full

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
