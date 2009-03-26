-- vim: set et ts=4 sw=4 foldmethod=marker:
-- {{{ Imports 
import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SinkAll

import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.EwmhDesktops
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
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Tabbed
import XMonad.Layout.WorkspaceDir

import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Themes

import Control.Arrow ((>>>), second)
import Data.Char
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Network
import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
-- }}}

-- {{{ main
main = do
    let myFont       = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
    let myWorkspaces = map show [1..9]

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
            , terminal           = "exec pterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = myWorkspaces
            , modMask            = mod4Mask
            , layoutHook         = {- ewmhDesktopsLayout -} myLayoutHook
            , manageHook         = myManageHook <+> manageDocks
            , logHook            = {- ewmhDesktopsLogHook >> -} myLogHook
            }

    let myKeys = 
            [ ("M-`",              spawn $ terminal myConfig)
            , ("M-S-`",            do viewEmptyWorkspace; spawn $ terminal myConfig)
            , ("M-c",              kill)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-<Return>",     windows W.focusMaster)
            , ("M-S-b",            sendMessage ToggleStruts)
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              scriptPrompt   myXPConfig)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-b",              bookmarkPrompt myXPConfig)
            , ("M-d",              changeDir      myXPConfig)
            , ("M-m",              withFocused (sendMessage . maximizeRestore))
            , ("M-S-m",            sendMessage Toggle)
            , ("M-<Pause>",        osdc "vol mute")
            , ("M-<Page_Up>",      osdc "vol up 10")
            , ("M-<Page_Down>",    osdc "vol down 10")
            , ("M-z",              spawn "exec xlock")
            , ("M-0",              viewEmptyWorkspace)
            , ("M-S-0",            tagToEmptyWorkspace)
            , ("M-S-t",            sinkAll)
            ] 
            ++ [ ("M-" ++ [key], windows . W.view $ tag) | (key,tag) <- zip "123456789" myWorkspaces ]
    let mediaKeys = []

    xmonad $ myConfig `additionalKeysP` myKeys `additionalKeys` mediaKeys
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
    , className =? "Gimp"               --> doFloat
    , className =? "Glade-3"            --> doFloat
    , className =? "Firefox-bin"        --> doF (W.shift "9")
    , className =? "Iceweasel"          --> doF (W.shift "9")
    , className =? "Navigator"          --> doF (W.shift "9")
    , resource  =? "mutt"               --> doF (W.shift "1")
    , resource  =? "offlineimap"        --> doF (W.shift "1")
    , resource  =? "irc"                --> doF (W.shift "1")
    , resource  =? "rss"                --> doF (W.shift "1")
    , title     =? "Factor workspace"   --> doFloat
    , resource  =? "desktop_window"     --> doIgnore
    , className =? "WMClock"            --> doIgnore
    , className =? "stalonetray"        --> doIgnore
    , className =? "kxdocker"           --> doIgnore
    , resource  =? "kdesktop"           --> doIgnore
    , resource  =? "kicker"             --> doIgnore ]
-- }}}

-- {{{ layout hook:
myLayoutHook = workspaceDir "~" 
             $ smartBorders
             $ avoidStruts 
             $ onWorkspace "1" grid
             -- $ onWorkspace "8" im
             $ onWorkspace "9" full
             $ tall ||| Mirror tall ||| grid ||| full
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

     -- full layout, renamed.
     full = named "Full" $ layoutHints Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
-- }}}

-- {{{ log hook
myLogHook = dynamicLogWithPP 
          $ xmobarPP { ppTitle   = xmobarColor "white" "" . shorten 100
                     , ppLayout  = layout 
                     , ppCurrent = xmobarColor "yellow" ""
                     , ppHidden  = xmobarColor "LightSlateBlue" ""
                     , ppHiddenNoWindows = xmobarColor "DarkSlateBlue" "" -- . wrap " " " "
                     , ppWsSep   = " "
                     , ppSep     = " | "
                     , ppOutput  = \x -> putStrLn $ ' ' : x
                     }
    where layout = xmobarColor "SteelBlue3" ""
          wrap b e x = b ++ x ++ e
          -- crap color b e = wrap (color b) (color e)

-- }}}

osdc s = io $ do-- {{{
    h <- connectTo "localhost" (PortNumber 8007)
    hPutStrLn h s
    hFlush h
    hClose h
-- }}}

scriptPrompt conf = do-- {{{
    dir <- io $ home "/.xmonad/scripts"
    dirExecPromptNamed conf spawn dir  "Script: "
-- }}}

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
