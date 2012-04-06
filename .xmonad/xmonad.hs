-- vim: set et ts=4 sw=4 foldmethod=marker:
{-# LANGUAGE FlexibleContexts #-}

-- {{{ Imports 
import XMonad hiding ( (|||) )
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.DwmPromote
import XMonad.Actions.SinkAll
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FocusNth
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import qualified XMonad.Actions.PhysicalScreens as PS

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Directory
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppLauncher

import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.OneBig
import XMonad.Layout.Fullscreen
import qualified XMonad.Layout.Fullscreen as FS

import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Run

import Control.Arrow ((>>>), second)
import Control.Monad (liftM, liftM2)
import Data.Char
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Maybe

-- next two lines are for myIsFullscreen.
import Data.Bits ((.&.))
import XMonad.Util.WindowProperties (getProp32s)

import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
-- }}}

-- Misc {{{
spawnExec str = spawn ("exec " ++ str ++ " &> /dev/null")

home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path)

progs = [ "capture-screen"
        , "chrome"
        , "chrome -p work"
        , "dterm"
        , "eclipse"
        , "firefox"
        , "lofn-chat"
        , "lofn-mail"
        , "midori"
        , "pwsafe-query"
        , "skype"
        , "xdo-paste"
        ]
-- }}}

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

    let myGSConfig x = x { gs_cellheight = 30
                         , gs_cellwidth = 150 
                         , gs_cellpadding = 5
                         }

    let myGSConfigWin = myGSConfig $ buildDefaultGSConfig fromClassName
    let myGSConfigStr = myGSConfig $ defaultGSConfig { gs_navigate = navNSearch }

    let dc = desktopConfig
    let myConfig = dc
            { borderWidth        = 2
            , terminal           = "dterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = map show [1..9]
            , modMask            = mod4Mask
            , manageHook         = fullscreenManageHook <+>  myManageHook <+> manageHook dc
            , logHook            = myLogHook <+> logHook desktopConfig
            , handleEventHook    = fullscreenEventHook <+> handleEventHook dc
            , layoutHook         = desktopLayoutModifiers (fullscreenFull myLayoutHook)
            , startupHook        = myStartupHook >> startupHook dc
            , focusFollowsMouse  = True
            }
    let myKeys = 
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-<Escape>",       spawnExec $ terminal myConfig)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-t",            withFocused $ windows . W.sink)
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              spawnSelected myGSConfigStr progs)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-o",              bookmarkPrompt myXPConfig)
            , ("M-q",              spawn "true")
            , ("M-g",              goToSelected myGSConfigWin)
            , ("M-b",            bringSelected myGSConfigWin)
            , ("M-S-c",            kill)
            , ("M-w",              PS.viewScreen 0)
            , ("M-e",              PS.viewScreen 1)
            , ("M-S-w",            PS.sendToScreen 0)
            , ("M-S-e",            PS.sendToScreen 1)
            , ("M-0",              gridselectWorkspace myGSConfigStr W.greedyView)
            , ("M-f",              sendMessage ToggleStruts)
            ] 

    let conf = myConfig `additionalKeysP` myKeys
    xmonad =<< dzen conf

-- }}}

-- {{{ manage hook:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
myManageHook = composeOne
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
             , resource  =? "chat"               -?> viewShift "chat"
             , resource  =? "kicker"             -?> doIgnore
             , myIsFullscreen                    -?> doFullFloat
             ]
    where viewShift = doF . liftM2 (.) W.view W.shift
-- }}}

-- {{{ layout hook:
myLayoutHook = lessBorders (Combine Difference Screen OnlyFloat)
             $ (tall ||| Mirror tall ||| onebig ||| grid ||| full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tall = named "Tall" 
          $ layoutHintsToCenter
          $ Tall nmaster delta ratio

     -- default grid
     grid = named "Grid" 
          $ layoutHintsToCenter
          $ Grid

     -- full layout, renamed.
     full = named "Full" 
          $ Full

     onebig = named "OneBig"
            $ layoutHints 
            $ OneBig (3/4) (3/4)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 7/12

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
-- }}}

myLogHook = do home <- io $ getEnv "HOME"
               updatePointer (Relative 0.5 0.5)

scriptPrompt conf = do-- {{{
    dir <- io $ home "/.xmonad/scripts"
    dirExecPromptNamed conf spawnExec dir  "Script: "
-- }}}

data Bookmark = Bookmark-- {{{

instance XPrompt Bookmark where
    showXPrompt Bookmark = "Bookmark: "
    completionToCommand _ = id

bookmarkPrompt :: XPConfig -> X ()
bookmarkPrompt c = do
    file  <- io $ getEnv "BOOKMARKS" `catch` const (home ".surfraw.bookmarks")
    marks <- io $ getBookmarks file
    mkXPrompt Bookmark c (mkComplFunFromList' (map fst marks)) (gotoBookmark marks)

gotoBookmark :: [(String, String)] -> String -> X ()
gotoBookmark marks name =
    case lookup name marks of
         Just url -> spawnExec ("url-handler " ++ url)
         Nothing  -> return ()

getBookmarks :: String -> IO [(String, String)]
getBookmarks file = do
    text <- readFile file
    return (map pair (filter (not.null) (lines text)))

pair :: String -> (String, String)
pair = break isSpace >>> second (dropWhile isSpace)
-- }}}

myIsFullscreen = do w <- ask
                    fs <- isFullscreen
                    if fs then return fs
                          else liftX $ do p <- getProp32s "_MOTIF_WM_HINTS" w
                                          case p of 
                                               Just (flags:_:decorations:_) -> return ((flags .&. 2) /= 0 && decorations == 0)
                                               Nothing -> return False


myStartupHook = do setWMName "LG3D"
                   spawn "be trayer"
