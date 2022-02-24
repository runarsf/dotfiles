-- Imports {{{
import XMonad
import qualified XMonad.StackSet as W

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
-- }}}

-- Layout {{{
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
-- }}}
-- }}}

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask            = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook         = myLayout      -- Use custom layouts
    , manageHook         = myManageHook  -- Match on certain windows
    , terminal           = "alacritty"
    , focusFollowsMouse  = True
    , clickJustFocuses   = False
    , borderWidth        = 3
    , workspaces         = ["1","2","3","4","5","6","7","8","9", "10"]
    , normalBorderColor  = "#2e2e2e"
    , focusedBorderColor = "#ebdbb2"
    -- , keys               = 
    -- , mouseBindings      = 
    -- , handleEventHook    = 
    -- , startupHook        = 
    }
  `additionalKeysP` -- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
    [ ("M-q"       , kill                                                                                           )
    , ("M-S-r"     , spawn "xmonad --recompile && xmonad --restart"                                                 )
    , ("M-<Return>", spawn "${TERMINAL:-alacritty}"                                                                 )
    , ("M-d"       , spawn "rofi -show"                                                                             )
    , ("M-S-d"     , spawn "dmenu_run"                                                                              )
    , ("M-S-c"     , spawn "toggleprogram picom --config \"${HOME}/.config/picom.conf\" -fcCGb --xrender-sync-fence")
    , ("M-x"       , spawn "betterlockscreen --lock dimblur --blur 8"                                               )
    , ("M1-p"      , spawn "screenshot -m region -t -c -o 'screenshot-xbackbone'"                                   )
    , ("M1-S-p"    , spawn "screenshot -m region -c"                                                                )
    , ("M-<Space>" , sendMessage NextLayout                                                                         )
    , ("M-r"       , refresh                                                                                        )
    , ("M-<Tab>"   , windows W.focusDown                                                                            )
    , ("M-j"       , windows W.focusDown                                                                            )
    , ("M-<Down>"  , windows W.focusDown                                                                            )
    , ("M-k"       , windows W.focusUp                                                                              )
    , ("M-<Up>"    , windows W.focusUp                                                                              )
    , ("M-m"       , windows W.focusMaster                                                                          )
    , ("M-S-j"     , windows W.swapDown                                                                             )
    , ("M-S-<Down>", windows W.swapDown                                                                             )
    , ("M-S-k"     , windows W.swapUp                                                                               )
    , ("M-S-<Up>"  , windows W.swapUp                                                                               )
    , ("M-h"       , sendMessage Shrink                                                                             )
    , ("M-l"       , sendMessage Expand                                                                             )
    , ("M-,"       , sendMessage (IncMasterN 1)                                                                     )
    , ("M-."       , sendMessage (IncMasterN (-1))                                                                  )
    , ("M-b"       , sendMessage ToggleStruts                                                                       )
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.5 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
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
