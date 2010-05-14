-- vim: set et ts=4 sw=4 foldmethod=marker:
-- {{{ Imports 
import XMonad hiding ( (|||) )
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.DwmPromote
import XMonad.Actions.SinkAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks

import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Directory
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators

import XMonad.Util.EZConfig

import Control.Arrow ((>>>), second)
import Data.Char
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Maybe
import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
import Text.XHtml (tag, strAttr, renderHtml, (<<), (!), primHtml)
-- }}}

-- Misc {{{
wsHome   = "1"
wsVM     = "6"
wsGimp   = "7"
wsDocs   = "8"
wsWeb    = "9"

wsList  = map show [1 .. 9]
wsKeys  = [ (x,x) | x <- wsList ]

spawnExec str = spawn ("exec " ++ str)

home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path)
-- }}}

-- {{{ main
main = do
    let myFont     = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
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
            , terminal           = "dterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = wsList
            , modMask            = mod4Mask
            , layoutHook         = myLayoutHook
            , manageHook         = myManageHook <+> manageDocks
            , logHook            = myLogHook
            , startupHook        = setWMName "LG3D"
            }

    let myKeys = 
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-c",              kill)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-<Return>",     windows W.focusMaster)
            , ("M-b",              sendMessage ToggleStruts)
            , ("M-m",            withFocused (sendMessage . maximizeRestore))
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              scriptPrompt   myXPConfig)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-o",              bookmarkPrompt myXPConfig)
            , ("M-S-q",            spawn "gnome-session-save --gui --logout-dialog")
            , ("M-S-l",            spawn "gnome-screensaver-command -l")
            ] ++ [ ("M-"   ++ k,   windows (W.greedyView n))  | (k, n) <- wsKeys ]
              ++ [ ("M-S-" ++ k,   windows (W.shift      n))  | (k, n) <- wsKeys ]

    xmonad $ ewmh $ myConfig `additionalKeysP` myKeys
-- }}}

-- {{{ myLogHook
myLogHook = do home <- io $ getEnv "HOME"
               dynamicLogWithPP (panzenPP (home ++ "/.panzen"))
--- }}}

-- panzenPP {{{
panzenPP f = defaultPP { ppTitle   = panzenColor "white" . shorten 50
                       , ppVisible = panzenColor "orange"
                       , ppLayout  = panzenColor "SteelBlue3"
                       , ppCurrent = panzenColor "yellow"
                       , ppHidden  = panzenColor "LightSlateBlue"
                       , ppHiddenNoWindows = panzenColor "DarkSlateBlue"
                       , ppWsSep   = " "
                       , ppSep     = " | "
                       , ppOutput  = writeFile f . oneline . panzenBar . primHtml
                       }
    where color   = strAttr "color"
          font    = strAttr "font" "Terminus"
          bold    = strAttr "weight" "bold"
          span    = tag "span"
          panzenColor   c = show . (span ! [color c, font, bold] <<)
          panzenBar       = show . (span ! [font, bold] << )
          oneline xs      = [ x | x <- xs, x /= '\n' ] ++ "\n"
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
myManageHook = composeAll
    [ className =? "MPlayer"            --> doFloat
    , resource  =? "pwsafe_prompt"      --> doFloat
    , className =? "Glade-3"            --> doFloat
    , title     =? "Factor workspace"   --> doFloat
    , className =? "VirtualBox"         --> doF (W.shift wsVM)
    , className =? "Gimp"               --> doF (W.shift wsGimp)
    , resource  =? "mutt"               --> doF (W.shift wsHome)
    , resource  =? "offlineimap"        --> doF (W.shift wsHome)
    , resource  =? "irc"                --> doF (W.shift wsHome)
    , resource  =? "rss"                --> doF (W.shift wsHome)
    , resource  =? "shell_fm"           --> doF (W.shift wsHome)
    , className =? "Pidgin"             --> doF (W.shift wsHome)
    , title     =? "Buddy List"         --> doF (W.shift wsHome)
    , className =? "Xpdf"               --> doF (W.shift wsDocs)
    , className =? "OpenOffice.org 2.4" --> doF (W.shift wsDocs)
    , resource  =? "OpenOffice.org"     --> doF (W.shift wsDocs)
    , className =? "Firefox-bin"        --> doF (W.shift wsWeb)
    , className =? "Firefox"            --> doF (W.shift wsWeb)
    , className =? "Iceweasel"          --> doF (W.shift wsWeb)
    , className =? "Navigator"          --> doF (W.shift wsWeb)
    , className =? "Gran Paradiso"      --> doF (W.shift wsWeb)
    , className =? "Google-chrome"      --> doF (W.shift wsWeb)
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "WMClock"            --> doIgnore
    , className =? "stalonetray"        --> doIgnore
    , className =? "kxdocker"           --> doIgnore
    , resource  =? "gnome-panel"        --> doFloat
    , resource  =? "kdesktop"           --> doIgnore
    , resource  =? "kicker"             --> doIgnore ]
-- }}}

-- {{{ layout hook:
myLayoutHook = avoidStruts
             $ (tall ||| Mirror tall ||| grid ||| full ||| gimp)
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

     {-im   = named "IM" 
          $ layoutHints
          $ maximize
          $ withIM (1%6) (Title "Buddy List") 
          $ Grid-}

     gimp = named "Gimp"
          $ layoutHints
          $ withIM (0.13) (Role "gimp-toolbox")
          $ reflectHoriz 
          $ withIM (0.20) (Role "gimp-dock")
          $ Full

     -- full layout, renamed.
     full = named "Full" $ smartBorders $ layoutHints Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 7/12

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
-- }}}

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
