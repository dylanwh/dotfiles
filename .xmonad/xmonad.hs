-- vim: set et ts=4 sw=4:
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
-- import XMonad.Actions.FocusNth
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer


import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.Directory
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.DirExec
import XMonad.Prompt.XMonad

import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Magnifier
import XMonad.Layout.WindowNavigation

import XMonad.Util.Themes
import XMonad.Util.EZConfig

import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
import Network
import Control.Arrow ((>>>), second)
import Data.Char

main = xmonad $ myConfig `additionalKeysP` myKeys

myFont = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
myTheme = defaultTheme { fontName = myFont }

myConfig = defaultConfig
    { borderWidth        = 1
    , terminal           = "pterm"
    , workspaces         = map show [1..9]
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "blue"
    , modMask            = mod4Mask
    , layoutHook         = ewmhDesktopsLayout myLayoutHook
    , manageHook         = myManageHook <+> manageDocks
    , logHook            = do -- ewmhDesktopsLogHook
                              -- updatePointer (Relative 0.5 0.5)
                              dynamicLog
    }

myKeys =
    [ ("M-`",              spawn $ XMonad.terminal myConfig)
    , ("M-S-`",            do viewEmptyWorkspace; spawn $ XMonad.terminal myConfig)
    , ("M-c",              kill)
    , ("M-<Return>",       dwmpromote)
    , ("M-S-<Return>",     windows W.focusMaster)
    , ("M-S-b",            sendMessage ToggleStruts)
    , ("M-n",              viewEmptyWorkspace)
    , ("M-S-n",            tagToEmptyWorkspace)
    , ("M-[",              prevWS)
    , ("M-]",              nextWS)
    , ("M-<Left>",         sendMessage $ Go L)
    , ("M-<Right>",        sendMessage $ Go R)
    , ("M-<Down>",         sendMessage $ Go D)
    , ("M-<Up>",           sendMessage $ Go U)
    , ("M-S-<Left>",       sendMessage $ Swap L)
    , ("M-S-<Right>",      sendMessage $ Swap R)
    , ("M-S-<Down>",       sendMessage $ Swap D)
    , ("M-S-<Up>",         sendMessage $ Swap U)
    , ("M-s",              sshPrompt myXPConfig)
    , ("M-p",              scriptPrompt myXPConfig)
    , ("M-S-p",            shellPrompt myXPConfig)
    , ("M-x",              xmonadPrompt myXPConfig)
    , ("M-o",              bookmarkPrompt myXPConfig)
    , ("M-d",              changeDir myXPConfig)
    , ("M-m",              withFocused (sendMessage . maximizeRestore))
    , ("M-S-m",            sendMessage Toggle)
    , ("M-g",              windowPromptGoto myXPConfig)
    , ("M-b",              windowPromptBring myXPConfig)
    , ("M-<Pause>",        osdc "vol mute")
    , ("M-<Page_Up>",      osdc "vol up 10")
    , ("M-<Page_Down>",    osdc "vol down 10")
    , ("M-<F12>",          spawn "xlock")
    , ("M1-<F2>",          shellPrompt myXPConfig)
    , ("M1-C-l",           spawn "xlock")
    , ("M-M1-C-s",         spawn "super shutdown -h now") ]

myXPConfig = defaultXPConfig
    { font              = fontName myTheme
    , height            = 24
    , bgColor           = "black"
    , fgColor           = "#A8A8A8"
    , borderColor       = "blue"
    , bgHLight          = "black"
    , fgHLight          = "white"
    }

-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayoutHook 
    = workspaceDir "~" 
    $ windowNavigation
    $ avoidStruts 
    $ layoutHints 
    $ maximize
    $ smartBorders 
    $ tall ||| Mirror tall ||| magnifier Grid ||| tabbed shrinkText myTheme ||| Full
    
  where
     -- default tiling algorithm partitions the screen into two panes
     tall   =  magnifier' (Tall nmaster delta ratio)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
myManageHook = composeAll
    [ className =? "MPlayer"            --> doFloat
    , className =? "Gimp"               --> doFloat
    , className =? "Glade-3"            --> doFloat
    , className =? "Firefox-bin"        --> doF (W.shift "9")
    , className =? "Iceweasel"          --> doF (W.shift "9")
    , resource  =? "mutt"               --> doF (W.shift "1")
    , resource  =? "mail"               --> doF (W.shift "1")
    , resource  =? "offlineimap"        --> doF (W.shift "1")
    , resource  =? "irc"                --> doF (W.shift "1")
    , resource  =? "rss"                --> doF (W.shift "1")
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "WMClock"            --> doIgnore
    , className =? "stalonetray"        --> doIgnore
    , resource  =? "kdesktop"           --> doIgnore
    , className =? "kxdocker"           --> doIgnore
    , resource  =? "kicker"             --> doIgnore ]

osdc s = io $ do
    h <- connectTo "localhost" (PortNumber 8007)
    hPutStrLn h s
    hFlush h
    hClose h

scriptPrompt conf = do
    dir <- io $ home "/.xmonad/scripts"
    dirExecPromptNamed myXPConfig spawn dir  "Script: "

home :: String -> IO String
home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path) 

data Bookmark = Bookmark

instance XPrompt Bookmark where
    showXPrompt Bookmark = "Bookmark: "
    completionToCommand _ = id

bookmarkPrompt :: XPConfig -> X ()
bookmarkPrompt c = do
    file  <- io $ getEnv "BOOKMARKS" `catch` const (home "pim/bookmarks")
    marks <- io $ getBookmarks file
    mkXPrompt Bookmark c (mkComplFunFromList' (map fst marks)) (gotoBookmark marks)

gotoBookmark :: [(String, String)] -> String -> X ()
gotoBookmark marks name =
    case lookup name marks of
         Just url -> spawn ("firefox " ++ url)
         Nothing  -> return ()

getBookmarks :: String -> IO [(String, String)]
getBookmarks file = do
    text <- readFile file
    return (map pair (filter (not.null) (lines text)))

pair :: String -> (String, String)
pair = break isSpace >>> second (dropWhile isSpace)

