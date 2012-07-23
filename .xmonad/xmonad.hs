-- vim: set et ts=4 sw=4 foldmethod=marker:
{-# LANGUAGE FlexibleContexts #-}

-- {{{ Imports 
import Control.Monad (liftM, liftM2)
import Data.Bits ((.&.))
import Data.Char
import Data.Maybe
import Data.Ratio ((%))
import System.Environment (getEnv)
import System.Exit

import XMonad hiding ( (|||), Tall )
import XMonad.Config.Gnome (gnomeRegister)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

{- Actions {{{ -}
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WorkspaceNames
import qualified XMonad.Actions.PhysicalScreens as PS
{- }}} -}

{- Hooks {{{ -}
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.RestoreMinimized
import XMonad.Hooks.SetWMName
{- }}} -}

{- Layouts {{{ -}
import XMonad.Layout.Column
import XMonad.Layout.DecorationAddons
import XMonad.Layout.Fullscreen
import XMonad.Layout.HintedTile
import XMonad.Layout.IM 
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.BorderResize
{- }}} -}

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties (getProp32s)
{- }}} -}

-- {{{ main
main = do
    let myFont      = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
        mySmallFont = "-xos4-terminus-bold-r-*-*-*-120-100-100-*-*-iso8859-1"
        myXPConfig  = defaultXPConfig
            { font        = myFont
            , height      = 24
            , bgColor     = "black"
            , fgColor     = "#A8A8A8"
            , borderColor = "red"
            , bgHLight    = "white"
            , fgHLight    = "black"
            }
        myGSConfig x = x { gs_cellheight  = 26
                         , gs_cellwidth   = 150 
                         , gs_cellpadding = 1
                         , gs_font        = mySmallFont
                         }
        gsWindows  = myGSConfig $ buildDefaultGSConfig fromClassName
        gsPrograms = myGSConfig $ defaultGSConfig { gs_navigate = navNSearch }

        myConfig = defaultConfig
            { borderWidth        = 2
            , terminal           = "dterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = map show [1..9]
            , modMask            = mod4Mask
            , manageHook         = myManageHook
            , logHook            = myLogHook
            , handleEventHook    = myHandleEventHook
            , layoutHook         = myLayoutHook
            , startupHook        = myStartupHook
            , focusFollowsMouse  = True
            }

        myKeys =
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-<Escape>",       spawnExec $ terminal myConfig)
            , ("M-<Return>",       dwmpromote)
            , ("M-p",              programMenu gsPrograms)
            , ("M-S-p",            spawn "pwsafe-query")
            , ("M-S-v",            spawn "xdo-paste")
            , ("M-g",              goToSelected gsWindows)
            , ("M-b",              bringSelected gsWindows)
            , ("M-S-c",            kill)
            , ("M-q",              PS.viewScreen 0)
            , ("M-w",              PS.viewScreen 1)
            , ("M-e",              PS.viewScreen 2)
            , ("M-r",              PS.viewScreen 3)
            , ("M-S-q",            PS.sendToScreen 0)
            , ("M-S-w",            PS.sendToScreen 1)
            , ("M-S-e",            PS.sendToScreen 2)
            , ("M-S-r",            PS.sendToScreen 3)
            , ("M-C-S-q",          io (exitWith ExitSuccess))
            , ("M-f",              sendMessage ToggleStruts)
            , ("M-m",              withFocused (sendMessage . maximizeRestore))
            , ("M-x",              spawn "mpc toggle")
            ]

        conf = ewmh (myConfig `additionalKeysP` myKeys)

    -- hack to make minecraft work: setWMName "LG3D" in loghook
    xmonad conf { logHook = logHook conf >> setWMName "LG3D" }
{- }}} -}

-- {{{ manage hook:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
myManageHook = fullscreenManageHook <+> manageDocks <+> rules
    where rules = composeOne
                [ appName   =? "pnmixer"            -?> doFloat
                , transience
                , myIsFullscreen                    -?> doFullFloat
                , className =? "Glade-3"            -?> doFloat
                , className =? "WMClock"            -?> doIgnore
                , className =? "stalonetray"        -?> doIgnore
                , className =? "kxdocker"           -?> doIgnore
                , className =? "trayer"             -?> doIgnore
                , resource  =? "pwsafe-prompt"      -?> doFloat
                , appName =? "google-chrome"        -?> doShift "9"
                , appName =? "Google-chrome (/home/dylan/.config/google-chrome-work)" -?> doShift "8"
                ]
{- }}} -}

{- {{{ layout hook: -}
-- $ layoutHintsToCenter

myLayoutHook = smartBorders 
             $ fullscreenFull
             $ avoidStruts
             $ onWorkspaces ["8", "9"] (chromeLayout 1.2 ||| layoutHintsToCenter Full)
             $ tiledLayout Tall ||| tiledLayout Wide ||| layoutHintsToCenter Full ||| floatLayout ||| gimpLayout

gimpLayout = renamed [CutWordsLeft 5, PrependWords "Gimp"] 
           $ withIM 0.11 (Role "gimp-toolbox") 
           $ reflectHoriz
           $ withIM 0.15 (Role "gimp-dock")
           $ trackFloating
           $ reflectHoriz simpleTabbed

floatLayout = renamed [CutWordsLeft 4, PrependWords "Float"] 
            $ imageButtonDeco shrinkText defaultThemeWithImageButtons 
            $ borderResize 
            $ maximize 
            $ minimize 
            $ positionStoreFloat

chromeLayout = renamed [CutWordsLeft 4, PrependWords "Chrome"] 
            . maximize 
            . minimize 
            . withIM ratio query . limitSlice limit . Column
    where ratio = 4/6
          query = ClassName "Google-chrome"
          limit = 3

tiledLayout = renamed [CutWordsLeft 2]
            . maximize 
            . minimize 
            . HintedTile nmaster delta ratio TopLeft
    where nmaster = 1
          ratio   = 89/150
          delta   = 1/100

{- }}} -}

{- {{{ log hook -}
myLogHook = do updatePointer (Relative 0.5 0.5)
               pp     <- workspaceNamesPP myPP
               status <- dynamicLogString pp
               xmonadPropLog status
               takeTopFocus
    where myPP = dzenPP { ppTitle = ppTitle dzenPP .  shorten 120 }
{- }}} -}

{- {{{ startup hook: -}
myStartupHook = do xmonadrc <- home ".xmonad/xmonadrc"
                   setWMName "LG3D"
                   spawnOnce xmonadrc
                   gnomeRegister
                   setWorkspaceName "1" ""
                   setWorkspaceName "8" ""
                   setWorkspaceName "9" ""
{- }}} -}

{- {{{ Event hook: -}
myHandleEventHook = fullscreenEventHook 
                <+> minimizeEventHook 
                <+> docksEventHook
                <+> restoreMinimizedEventHook 
{- }}} -}

-- utilility functions {{{
spawnExec str = spawn ("exec " ++ str ++ " &> /dev/null")

home path = do dir <- io $ getEnv "HOME"
               return (dir ++ '/' : path)

myIsFullscreen = do w <- ask
                    fs <- isFullscreen
                    if fs then return fs
                          else liftX $ do p <- getProp32s "_MOTIF_WM_HINTS" w
                                          case p of 
                                               Just (flags:_:decorations:_) -> return ((flags .&. 2) /= 0 && decorations == 0)
                                               Nothing -> return False

programMenu gs = do progs_file <- home ".xmonad/programs"
                    progs      <- liftM lines (io $ readFile progs_file)
                    spawnSelected gs progs
 
-- }}}
