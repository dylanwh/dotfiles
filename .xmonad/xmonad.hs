--
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
import XMonad.Actions.FocusNth
import XMonad.Actions.Submap
import XMonad.Actions.Search

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.Directory
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.DirExec

import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.NoBorders
import XMonad.Layout.Dishes
import XMonad.Util.Themes


import XMonad.Util.EZConfig

import System.Environment (getEnv)
import System.IO (hPutStrLn, hClose, hFlush)
import Network

main = xmonad $ myConfig `additionalKeysP` myKeys

myConfig = defaultConfig
    { borderWidth        = 1
    , terminal           = "pterm"
    , workspaces         = map show [1..9] ++ ["wyrd"]
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "blue"
    , modMask            = mod4Mask
    , layoutHook         = ewmhDesktopsLayout myLayoutHook
    , manageHook         = myManageHook <+> manageDocks
    , logHook            = ewmhDesktopsLogHook >> dynamicLog }

myKeys =
    [ ("M-`",            spawn $ XMonad.terminal myConfig)
    , ("M-S-`",          do viewEmptyWorkspace; spawn $ XMonad.terminal myConfig)
    , ("M-c",            kill)
    , ("M-<Return>",     dwmpromote)
    , ("M-S-<Return>",   windows W.focusMaster)
    , ("M-S-b",          sendMessage ToggleStruts)
    , ("M-n",            viewEmptyWorkspace)
    , ("M-S-n",          tagToEmptyWorkspace)
    , ("M-p",            shellPrompt myXPConfig)
    , ("M-s",            sshPrompt myXPConfig)
    , ("M-m",            withFocused (sendMessage . maximizeRestore))
    , ("M-g",            windowPromptGoto myXPConfig)
    , ("M-b",            windowPromptBring myXPConfig)
    , ("M-<Pause>",      osdc "vol mute")
    , ("M-<Page_Up>",    osdc "vol up 10")
    , ("M-<Page_Down>",  osdc "vol down 10")
    , ("M-d",            changeDir myXPConfig)
    , ("M-f g",          promptSearch myXPConfig "firefox" google)
    , ("M-;",            scriptMenu)
    , ("M-w",            windows $ W.greedyView "wyrd")]

myXPConfig = defaultXPConfig
    { font              = "-xos4-terminus-bold-r-*-*-*-140-100-100-*-*-iso8859-1"
    , height            = 24 {-
    , promptBorderWidth = myBorderWidth
    , bgColor           = "black"
    , fgColor           = "white"
    , borderColor       = "blue"
    , bgHLight          = "blue"
    , fgHLight          = "white" -}
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
myLayoutHook = --workspaceDir "~" 
    {-$-} avoidStruts 
    $ layoutHints 
    $ maximize
    $ smartBorders 
    $ tall ||| Mirror tall ||| Grid ||| Dishes 2 (1/7) ||| Full
    
  where
     -- default tiling algorithm partitions the screen into two panes
     tall   = Tall nmaster delta ratio

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
    -- , className =? "Zenity"             --> doFloat
    , className =? "Firefox-bin"        --> doF (W.shift "2")
    , resource  =? "mutt"               --> doF (W.shift "1")
    , resource  =? "mail"               --> doF (W.shift "1")
    , resource  =? "offlineimap"        --> doF (W.shift "1")
    , resource  =? "irc"                --> doF (W.shift "1")
    , resource  =? "rss"                --> doF (W.shift "1")
    , resource  =? "wyrd"               --> doF (W.shift "wyrd")
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


scriptMenu = do home <- liftIO $ getEnv "HOME"
		let dir = home ++ "/.xmonad/scripts"
		dirExecPromptNamed myXPConfig spawn dir  "script: "
