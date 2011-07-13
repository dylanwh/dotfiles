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
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FocusNth
import XMonad.Actions.CycleWS
import XMonad.Hooks.FadeInactive
import qualified XMonad.Actions.PhysicalScreens as PS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleFloat
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.OneBig

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
import Text.XHtml (tag, strAttr, renderHtml, (<<), (!), primHtml)
-- }}}

-- Misc {{{
spawnExec str = spawn ("exec " ++ str ++ " &> /dev/null")

home path = do dir <- io $ getEnv "HOME" `catch` const (return "/")
               return (dir ++ '/' : path)
-- }}}

fromResource :: Window -> Bool -> X (String, String)
fromResource w active = runQuery resource w >>= flip defaultColorizer active

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
    let myGSConfig = (buildDefaultGSConfig fromClassName) 
            { gs_cellheight = 30
            , gs_cellwidth = 100 
            , gs_cellpadding = 5
            }

    let myConfig = ewmh $ defaultConfig
            { borderWidth        = 2
            , terminal           = "dterm"
            , normalBorderColor  = "#000033"
            , focusedBorderColor = "red"
            , workspaces         = ["chat", "web"]
            , modMask            = mod4Mask
            , layoutHook         = myLayoutHook
            , manageHook         = myManageHook
            , startupHook        = myStartupHook
            , logHook            = myLogHook
            , focusFollowsMouse  = True
            }

    let myKeys = 
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-<Escape>",       spawnExec $ terminal myConfig)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-t",            withFocused $ windows . W.sink)
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              scriptPrompt   myXPConfig)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-o",              bookmarkPrompt myXPConfig)
            , ("M-S-q",            spawn "gnome-session-save --gui --logout-dialog")
            , ("M-S-l",            spawn "gnome-screensaver-command -l")
            , ("M-g",              goToSelected myGSConfig)
            , ("M-S-g",              bringSelected myGSConfig)
            , ("M-<Left>",         spawn "x2 prev")
            , ("M-<Right>",        spawn "x2 next")
            , ("M-x",              spawn "x2 toggle 2> /tmp/foo") 
            , ("M-0",              selectWorkspace myXPConfig)
            , ("M-S-0",            withWorkspace myXPConfig (windows . W.shift))
            , ("M-f",              selectWorkspace myXPConfig)
            , ("M-S-f",            withWorkspace myXPConfig (windows . W.shift))
            , ("M-r",              renameWorkspace myXPConfig)
            , ("M-S-r",            removeWorkspace)
            , ("M-S-c",            kill)
            , ("M-C-S-q",          spawn "be xcompmgr")
            , ("M-w",              PS.viewScreen 0)
            , ("M-e",              PS.viewScreen 1)
            , ("M-S-w",            PS.sendToScreen 0)
            , ("M-S-e",            PS.sendToScreen 1)
            ] ++ [ ("M-" ++ show n, withNthWorkspace W.view $ n - 1) | n <- [1..9] ]
              ++ [ ("M-S-" ++ show n, withNthWorkspace W.shift $ n - 1) | n <- [1..9] ]

    xmonad =<< myXmobar (myConfig `additionalKeysP` myKeys)
-- }}}

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
myManageHook = composeOne
             [ transience
             , myIsFullscreen                    -?> doFullFloat
             , title     =? "Factor workspace"   -?> doFloat
             , className =? "Glade-3"            -?> doFloat
             , className =? "WMClock"            -?> doIgnore
             , className =? "stalonetray"        -?> doIgnore
             , className =? "kxdocker"           -?> doIgnore
             , className =? "trayer"             -?> doIgnore
             , className =? "Google-chrome"      -?> doShift "web"
             , resource  =? "pwsafe-prompt"      -?> doFloat
             , resource  =? "desktop_window"     -?> doIgnore
             , resource  =? "gnome-panel"        -?> doIgnore
             , resource  =? "tint2"              -?> doIgnore
             , resource  =? "kdesktop"           -?> doIgnore
             , resource  =? "panel"              -?> doIgnore
             , resource  =? "chat"               -?> viewShift "chat"
             , resource  =? "kicker"             -?> doIgnore
             ]
    where viewShift = doF . liftM2 (.) W.view W.shift
-- }}}

-- {{{ layout hook:
myLayoutHook = lessBorders OnlyFloat
             $ (tall ||| Mirror tall ||| onebig ||| grid ||| simpleFloat ||| full)
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

myXmobar :: LayoutClass l Window
         => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar conf = statusBar "be xmobar" xmobarPP toggleStrutsKey conf
    where toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myLogHook = do updatePointer (Relative 0.5 0.5)
               myFadeInactiveLogHook 0.6

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

myIsUnfocused = do ok <- check rules
                   if ok
                      then isUnfocused
                      else return False
    where check = liftM or . sequence
          rules = [ className =? "URxvt"
                  , className =? "Google-chrome"
                  ]

myFadeInactiveLogHook :: Rational -> X ()
myFadeInactiveLogHook = fadeOutLogHook . fadeIf myIsUnfocused

myStartupHook = do setWMName "LG3D"
                   spawn "be trayer & sleep 1; be xcompmgr"
                   spawn "xrdb-reload"

