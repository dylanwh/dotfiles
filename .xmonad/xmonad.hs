-- vim: set et ts=4 sw=4 foldmethod=marker:
{-# LANGUAGE FlexibleContexts #-}

-- {{{ Imports 
import XMonad hiding ( (|||) )
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified XMonad.Actions.PhysicalScreens as PS
import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowMenu

import XMonad.Config.Gnome (gnomeRegister)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.RestoreMinimized
import XMonad.Hooks.SetWMName

import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Control.Monad (liftM, liftM2)
import Data.Char
import Data.Ratio ((%))
import Data.Maybe

-- next two lines are for myIsFullscreen.
import Data.Bits ((.&.))
import XMonad.Util.WindowProperties (getProp32s)

import System.Environment (getEnv)
{- }}} -}

-- {{{ main
main = do
    let myFont     = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
    let myXPConfig = defaultXPConfig
            { font        = myFont
            , height      = 24
            , bgColor     = "black"
            , fgColor     = "#A8A8A8"
            , borderColor = "red"
            , bgHLight    = "white"
            , fgHLight    = "black"
            }

    let myGSConfig x = x { gs_cellheight  = 26
                         , gs_cellwidth   = 150 
                         , gs_cellpadding = 1
                         , gs_font        = "-xos4-terminus-bold-r-*-*-*-120-100-100-*-*-iso8859-1"
                         }

    let gsWindows    = myGSConfig $ buildDefaultGSConfig fromClassName
    let gsPrograms   = myGSConfig $ defaultGSConfig { gs_navigate = navNSearch }
    let gsWorkspaces = myGSConfig $ defaultGSConfig { gs_navigate = navNSearch }

    let myConfig = defaultConfig
            { borderWidth        = 2
            , terminal           = "dterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = map show [1..9]
            , modMask            = mod4Mask
            , manageHook         = fullscreenManageHook <+> myManageHook
            , logHook            = myLogHook
            , handleEventHook    = myHandleEventHook
            , layoutHook         = myLayoutHook
            , startupHook        = myStartupHook
            , focusFollowsMouse  = True
            }

    let myKeys = 
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-<Escape>",       programMenu gsPrograms)
            , ("M-<Return>",       dwmpromote)
            , ("M-p",              programMenu gsPrograms)
            , ("M-S-p",            shellPrompt myXPConfig)
            , ("M-g",              goToSelected gsWindows)
            , ("M-b",              bringSelected gsWindows)
            , ("M-S-c",            kill)
            , ("M-q",              spawn "true")
            , ("M-w",              PS.viewScreen 0)
            , ("M-e",              PS.viewScreen 1)
            , ("M-S-w",            PS.sendToScreen 0)
            , ("M-S-e",            PS.sendToScreen 1)
            , ("M-0",              gridselectWorkspace gsWorkspaces W.greedyView)
            , ("M-f",              sendMessage ToggleStruts)
            , ("M-o",              windowMenu)
            , ("M-S-v",            spawn "xdo-paste")
            , ("M-S-q",            spawn "gnome-session-quit --no-prompt")
            ] 

    let conf = ewmh (myConfig `additionalKeysP` myKeys)

    -- hack to make minecraft work?
    xmonad {- =<< dzen -} conf { logHook = logHook conf >> setWMName "LG3D" }
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
                [ transience
                , title     =? "Add to Panel"       -?> doFloat
                , resource  =? "gnome-panel"        -?> doIgnore
                , title     =? "Factor workspace"   -?> doFloat
                , className =? "Glade-3"            -?> doFloat
                , className =? "WMClock"            -?> doIgnore
                , className =? "stalonetray"        -?> doIgnore
                , className =? "kxdocker"           -?> doIgnore
                , className =? "trayer"             -?> doIgnore
                , className =? "Google-chrome"      -?> doShift "web"
                , resource  =? "pwsafe-prompt"      -?> doFloat
                , resource  =? "desktop_window"     -?> doIgnore
                , resource  =? "tint2"              -?> doIgnore
                , resource  =? "kdesktop"           -?> doIgnore
                , resource  =? "panel"              -?> doIgnore
                , resource  =? "kicker"             -?> doIgnore
                , myIsFullscreen                    -?> doFullFloat
                ]
{- }}} -}

{- {{{ layout hook: -}
myLayoutHook = smartBorders 
             $ fullscreenFull
             $ avoidStruts
             $ layoutHintsToCenter
             $ maximize
             $ minimize
             $ tall ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tall = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 7/12

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
{- }}} -}

{- {{{ log hook -}
myLogHook = do updatePointer (Relative 0.5 0.5)
               status <- dynamicLogString dzenPP
               xmonadPropLog status
               takeTopFocus
{- }}} -}

{- {{{ startup hook: -}
myStartupHook = do xmonadrc <- home ".xmonad/xmonadrc"
                   setWMName "LG3D"
                   spawnOnce xmonadrc
                   gnomeRegister
                   
{- }}} -}

myHandleEventHook = fullscreenEventHook 
                <+> minimizeEventHook 
                <+> restoreMinimizedEventHook 


-- utilility functions {{{
spawnExec str = spawn ("exec " ++ str ++ " &> /dev/null")

home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path)
programMenu gs = do progs_file <- home ".xmonad/programs"
                    progs      <- liftM lines (io $ readFile progs_file)
                    spawnSelected gs progs

myIsFullscreen = do w <- ask
                    fs <- isFullscreen
                    if fs then return fs
                          else liftX $ do p <- getProp32s "_MOTIF_WM_HINTS" w
                                          case p of 
                                               Just (flags:_:decorations:_) -> return ((flags .&. 2) /= 0 && decorations == 0)
                                               Nothing -> return False
-- }}}
