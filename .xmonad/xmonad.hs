-- vim: set et ts=4 sw=4 foldmethod=marker:
-- {{{ Imports 
import XMonad hiding ( (|||) )
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.SinkAll
import XMonad.Layout.SimpleFloat

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Directory
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Prompt.Layout

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Tabbed
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.LayoutCombinators


import XMonad.Util.EZConfig

import Control.Arrow ((>>>), second)
import Data.Char
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
-- }}}

-- {{{ main
main = do
    let myFont       = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
    let myWorkspaces = map show [ 1 .. 9 ]

    let myXPConfig = defaultXPConfig 
            { font        = myFont
            , height      = 24
            , bgColor     = "black"
            , fgColor     = "#A8A8A8"
            , borderColor = "blue"
            , bgHLight    = "black"
            , fgHLight    = "white"
            }

    let myConfig = defaultConfig
            { borderWidth        = 2
            , terminal           = "exec robo"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = myWorkspaces
            , modMask            = mod4Mask
            , layoutHook         = ewmhDesktopsLayout myLayoutHook
            , manageHook         = myManageHook <+> manageDocks
            , logHook            = ewmhDesktopsLogHook >> myLogHook
            }

    let myKeys = 
            [ ("M-`",              spawn $ terminal myConfig)
            , ("M-c",              kill)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-<Return>",     windows W.focusMaster)
            , ("M-S-b",            sendMessage ToggleStruts)
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              scriptPrompt   myXPConfig)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-o",              bookmarkPrompt myXPConfig)
            , ("M-d",              changeDir      myXPConfig)
            , ("M-m",              withFocused (sendMessage . maximizeRestore))
            , ("M-S-m",            sendMessage Toggle)
            , ("M-z",              spawn "exec xlock")
            , ("M-S-t",            sinkAll)
            , ("M-n",              viewEmptyWorkspace)
            , ("M-S-n",            tagToEmptyWorkspace)
            , ("M-<Tab>",          cycleRecentWS [xK_Super_L] xK_Tab xK_grave)
            , ("M-x",              layoutPrompt myXPConfig)
            , ("<F1>",             sendMessage (JumpToLayout "Tall"))
            , ("<F2>",             sendMessage (JumpToLayout "Mirror Tall"))
            , ("<F3>",             sendMessage (JumpToLayout "Full"))
            , ("<F4>",             sendMessage (JumpToLayout "Grid"))
            , ("<F5>",             sendMessage (JumpToLayout "Float"))
            , ("<F6>",             sendMessage (JumpToLayout "IM"))
            , ("<F7>",             sendMessage (JumpToLayout "Gimp"))
            ] 

    xmonad $ myConfig `additionalKeysP` myKeys
-- }}}

myLogHook = do status <- dynamicLogString defaultPP
               home   <- io $ getEnv "HOME"
               io $ writeFile (home ++ "/.xmonad/status") status

-- {{{ manage hook:
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
    , resource  =? "pwsafe"             --> doFloat
    , className =? "Glade-3"            --> doFloat
    , title     =? "Factor workspace"   --> doFloat
    , className =? "VirtualBox"         --> doF (W.shift "5")
    , resource  =? "mutt"               --> doF (W.shift "1")
    , resource  =? "offlineimap"        --> doF (W.shift "1")
    , resource  =? "irc"                --> doF (W.shift "1")
    , resource  =? "rss"                --> doF (W.shift "1")
    , className =? "Gimp"               --> doF (W.shift "5")
    , resource  =? "shell_fm"           --> doF (W.shift "7")
    , className =? "Xpdf"               --> doF (W.shift "8")
    , className =? "OpenOffice.org 2.4" --> doF (W.shift "8")
    , resource  =? "OpenOffice.org"     --> doF (W.shift "8")
    , className =? "Firefox-bin"        --> doF (W.shift "9")
    , className =? "Firefox"            --> doF (W.shift "9")
    , className =? "Iceweasel"          --> doF (W.shift "9")
    , className =? "Navigator"          --> doF (W.shift "9")
    , className =? "Gran Paradiso"      --> doF (W.shift "9")
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "WMClock"            --> doIgnore
    , className =? "stalonetray"        --> doIgnore
    , className =? "kxdocker"           --> doIgnore
    , resource  =? "gnome-panel"        --> doFloat
    , resource  =? "kdesktop"           --> doIgnore
    , resource  =? "kicker"             --> doIgnore ]
-- }}}

-- {{{ layout hook:
--             $ onWorkspace "gimp" gimp
myLayoutHook = workspaceDir "~" 
             $ avoidStruts 
             $ tall ||| Mirror tall ||| full ||| grid ||| float ||| im ||| gimp
  where
     -- default tiling algorithm partitions the screen into two panes
     tall = named "Tall" 
          $ layoutHints
          $ maximize
          $ Tall nmaster delta ratio

     -- default grid
     grid = named "Grid" 
          $ layoutHints
          $ maximize
          $ Grid

     -- im layout
     im   = named "IM" 
          $ reflectHoriz 
          $ withIM (1%6) (Title "Buddy List") 
          $ Grid

     gimp = named "Gimp"
          $ withIM (0.13) (Role "gimp-toolbox")
          $ reflectHoriz 
          $ withIM (0.20) (Role "gimp-dock")
          $ Full

     -- floating layout, renamed
     float = named "Float" $ layoutHints $ maximize $  simpleFloat

     -- full layout, renamed.
     full = named "Full" $ smartBorders $ layoutHints Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
-- }}}

scriptPrompt conf = do-- {{{
    dir <- io $ home "/.xmonad/scripts"
    dirExecPromptNamed conf execSpawn dir  "Script: "
-- }}}

execSpawn str = spawn ("exec " ++ str)

home :: String -> IO String-- {{{
home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path) -- }}}

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]-- {{{
replace from to [] = []
replace from to str@(x:xs)
    | from `isPrefixOf` str = to ++ replace from to (drop (length from) str)
    | otherwise             = x : replace from to xs


data Bookmark = Bookmark-- {{{

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
-- }}}
