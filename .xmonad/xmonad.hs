import XMonad
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
import XMonad.Layout.Maximize
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise

modm = mod4Mask

colorBlue = "#33aadd"
colorBlue1 = "#68CCD5"
colorBlue2 = "#2EB1BD"
colorBlue3 = "#02A3B2"
colorBlue4 = "#016E78"
colorBlue5 = "#01454C"

myFont = "xft:Source Han Code JP:pixelsize=10:antialias=true"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal = "urxvtc"
        , modMask  = modm
        , startupHook = setWMName "LG3D"
        , logHook = dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc }
        , focusedBorderColor = colorBlue
        }
        `additionalKeys`
        [ ((modm, xK_z), spawn "slock")
        , ((modm, xK_f), goToSelected myGSConfig)
        , ((modm .|. shiftMask, xK_p), runOrRaisePrompt myXPConfig)
        ]

myPP :: PP
myPP = defaultPP
    { ppSort = getSortByXineramaRule
    , ppCurrent = xmobarColor "#000000" colorBlue1 . pad
    , ppVisible = xmobarColor "#ffffff" colorBlue5 . pad
    , ppHidden  = xmobarColor "#ffffff" "#000000"  . pad
    , ppWsSep = ""
    , ppTitle = shorten 100
    }

myLogHook h = dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn h
    , ppSort = getSortByXineramaRule
    }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font = myFont
    , height = 20
    , position = Top
    }

myTabConfig :: Theme
myTabConfig = defaultTheme
    { fontName = myFont
    }

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig
    { gs_font = myFont
    }

