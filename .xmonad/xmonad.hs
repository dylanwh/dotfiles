-- vim: set et ts=4 sw=4 foldmethod=marker:
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
import XMonad.Layout.WorkspaceDir

import XMonad.Util.EZConfig

import Control.Arrow ((>>>), second)
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
    let myXPConfig' = myXPConfig { height = 28 }
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
            , workspaces         = ["chat", "code", "web"]
            , modMask            = mod4Mask
            , layoutHook         = myLayoutHook
            , manageHook         = myManageHook <+> manageDocks
            , logHook            = myLogHook
            , focusFollowsMouse  = True
            }

    let myKeys = 
            [ ("M-`",              spawnExec $ terminal myConfig)
            , ("M-c",              kill)
            , ("M-<Return>",       dwmpromote)
            , ("M-S-t",            withFocused $ windows . W.sink)
            , ("M-S-b",            sendMessage ToggleStruts)
            , ("M-s",              sshPrompt      myXPConfig)
            , ("M-p",              scriptPrompt   myXPConfig)
            , ("M-S-p",            shellPrompt    myXPConfig)
            , ("M-o",              bookmarkPrompt myXPConfig)
            , ("M-d",              changeDir myXPConfig)
            , ("M-S-q",            spawn "gnome-session-save --gui --logout-dialog")
            , ("M-S-l",            spawn "gnome-screensaver-command -l")
            , ("M-g",              goToSelected myGSConfig)
            , ("M-b",              bringSelected myGSConfig)
            , ("M-<Left>",         prevWS)
            , ("M-<Right>",        nextWS)
            , ("M-0",              selectWorkspace myXPConfig)
            , ("M-S-0",            withWorkspace myXPConfig (windows . W.shift))
            , ("M-f",              selectWorkspace myXPConfig)
            , ("M-S-f",            withWorkspace myXPConfig (windows . W.shift))
            , ("M-r",              renameWorkspace myXPConfig)
            , ("M-x",              removeWorkspace)
            ] ++ [ ("M-" ++ show n, withNthWorkspace W.greedyView $ n - 1) | n <- [1..9] ]
              ++ [ ("M-S-" ++ show n, withNthWorkspace W.shift $ n - 1) | n <- [1..9] ]

{-
   , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
   , ((modm .|. shiftMask, xK_v      ), selectWorkspace defaultXPConfig)
   , ((modm, xK_m                    ), withWorkspace defaultXPConfig (windows . W.shift))
   , ((modm .|. shiftMask, xK_m      ), withWorkspace defaultXPConfig (windows . copy))
   , ((modm .|. shiftMask, xK_r      ), renameWorkspace defaultXPConfig)
-}

    let myConf = myConfig { startupHook = startupHook myConfig >> setWMName "LG3D" }
    xmonad $ myConf `additionalKeysP` myKeys
-- }}}

-- {{{ myLogHook
myLogHook = do home <- io $ getEnv "HOME"
               dynamicLogWithPP (panzenPP (home ++ "/.panzen"))
               updatePointer (Relative 0.5 0.5)
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
myManageHook = composeOne
    [ transience
    , myIsFullscreen                    -?> (doF W.focusDown <+> doFullFloat)
    , resource  =? "pwsafe_prompt"      -?> doFloat
    , className =? "Glade-3"            -?> doFloat
    , title     =? "Factor workspace"   -?> doFloat
    , resource  =? "desktop_window"     -?> doIgnore
    , className =? "WMClock"            -?> doIgnore
    , className =? "stalonetray"        -?> doIgnore
    , className =? "kxdocker"           -?> doIgnore
    , resource  =? "gnome-panel"        -?> doFloat
    , resource  =? "kdesktop"           -?> doIgnore
    , resource  =? "kicker"             -?> doIgnore ]
-- }}}

-- {{{ layout hook:
myLayoutHook = avoidStruts
             $ workspaceDir "~"
             $ (tall ||| Mirror tall ||| onebig ||| grid ||| simpleFloat ||| full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tall = named "Tall" 
          $ lessBorders OnlyFloat
          $ layoutHints
          $ Tall nmaster delta ratio

     -- default grid
     grid = named "Grid" 
          $ lessBorders OnlyFloat
          $ layoutHints
          $ Grid

     -- full layout, renamed.
     full = named "Full" 
          $ lessBorders OnlyFloat
          $ layoutHints
          $ Full

     onebig = named "OneBig"
            $ lessBorders OnlyFloat
            $ layoutHints
            $ OneBig (3/4) (3/4)

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
         Just url -> spawnExec ("firefox " ++ url)
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
