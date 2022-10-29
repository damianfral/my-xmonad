{-# LANGUAGE TypeApplications #-}

import qualified Data.Map as M
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.Run (spawnPipe)

------------------------------------------------------------------------

enableLaptopMonitor :: X ()
enableLaptopMonitor = spawn "xrandr --output eDP-1  --auto"

disableLaptopMonitor :: X ()
disableLaptopMonitor = spawn "xrandr --output eDP-1  --off"

enableExternalMonitor :: X ()
enableExternalMonitor = spawn "xrandr --output HDMI-2 --auto"

disableExternalMonitor :: X ()
disableExternalMonitor = spawn "xrandr --output HDMI-2 --off"

------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "kitty"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--

myWorkspaces :: [String]
myWorkspaces = show @Int <$> [1 .. 9]

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = composeAll []

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts. Note that each layout is separated by |||,
-- which denotes layout choice.
--
defaultLayouts ::
  ModifiedLayout
    AvoidStruts
    ( Choose
        (ModifiedLayout SmartBorder Tall)
        (ModifiedLayout WithBorder Full)
    )
    Window
defaultLayouts =
  avoidStruts $ smartBorders (Tall 1 (3 / 100) (1 / 2)) ||| noBorders Full

------------------------------------------------------------------------
-- Colors and borders
--
myNormalBorderColor :: String
myNormalBorderColor = "#405c79"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bf8b56"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 2

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt"). You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { position = Top,
      font = "xft:Anonymice Nerd Font:pixelsize=12",
      bgColor = "#f7f9fb",
      fgColor = "#002b36",
      bgHLight = "#002b36",
      fgHLight = "#f7f9fb",
      height = 16
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask'}) =
  M.fromList $
    ----------------------------------------------------------------------
    -- Custom key bindings
    --

    -- Start a terminal. Terminal to start is specified by myTerminal variable.
    [ ((modMask' .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      -- Takes screenshot.
      ((0, 0x1008ff81), spawn "flameshot gui"),
      ( (modMask', xK_r),
        runOrRaisePrompt myPromptConfig
      ),
      -- Multimedia keys
      --
      -- XF86AudioLowerVolume
      ((0, 0x1008ff11), spawn "pulsemixer --change-volume -5 --max-volume 100"),
      -- XF86AudioRaiseVolume
      ((0, 0x1008ff13), spawn "pulsemixer --change-volume +5 --max-volume 100"),
      -- XF86AudioMute
      ((0, 0x1008ff12), spawn "pulsemixer --set-volume 0 --max-volume 100"),
      -- XF86AudioMute
      ((shiftMask, 0x1008ff12), spawn "pulsemixer --set-volume 100 --max-volume 100"),
      -- XF86AudioNext
      ((0, 0x1008ff17), spawn "playerctl next"),
      -- XF86AudioPrev
      ((0, 0x1008ff16), spawn "playerctl previous"),
      -- XF86AudioPlay
      ((0, 0x1008ff14), spawn "playerctl play-pause"),
      -- XF86AudioStop
      ((0, 0x1008ff15), spawn "playerctl stop"),
      ((modMask', xK_p), XMonad.Prompt.Pass.passPrompt myPromptConfig),
      ((modMask' .|. shiftMask, xK_p), XMonad.Prompt.Pass.passGeneratePrompt myPromptConfig),
      -- Eject CD tray.
      ((0, 0x1008FF2C), spawn "eject -T"),
      ((modMask', xK_e), enableExternalMonitor),
      ((modMask' .|. shiftMask, xK_e), disableExternalMonitor),
      ((modMask', xK_i), enableLaptopMonitor),
      ((modMask' .|. shiftMask, xK_i), disableLaptopMonitor),
      ((modMask', xK_f), sendMessage ToggleStruts),
      -- Lock and suspend/hibernate
      ((modMask' .|. shiftMask, xK_l), spawn "dm-tool lock"),
      ((modMask' .|. shiftMask, xK_s), spawn "systemctl suspend"),
      ((modMask' .|. shiftMask, xK_h), spawn "systemctl hybrid-sleep"),
      --------------------------------------------------------------------
      -- "Standard" xmonad key bindings
      --

      -- Close focused window.
      ((modMask' .|. shiftMask, xK_c), kill),
      -- Cycle through the available layout algorithms.
      ((modMask', xK_space), sendMessage NextLayout),
      -- Reset the layouts on the current workspace to default.
      ((modMask' .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size.
      ((modMask', xK_n), refresh),
      -- Move focus to the next window.
      ((modMask', xK_Tab), windows W.focusDown),
      -- Move focus to the previous window.
      ((modMask' .|. shiftMask, xK_Tab), windows W.focusUp),
      -- Move focus to the next window.
      ((modMask', xK_j), windows W.focusDown),
      -- Move focus to the previous window.
      ((modMask', xK_k), windows W.focusUp),
      -- Move focus to the master window.
      ((modMask', xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window.
      ((modMask', xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window.
      ((modMask' .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window.
      ((modMask' .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area.
      ((modMask', xK_h), sendMessage Shrink),
      -- Expand the master area.
      ((modMask', xK_l), sendMessage Expand),
      -- Push window back into tiling.
      ((modMask', xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area.
      ((modMask', xK_comma), sendMessage (IncMasterN 1)),
      -- Decrement the number of windows in the master area.
      ((modMask', xK_period), sendMessage (IncMasterN (-1))),
      -- Quit xmonad.
      ((modMask' .|. shiftMask, xK_q), io exitSuccess),
      -- Restart xmonad.
      ((modMask', xK_grave), toggleWS)
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [ ((m .|. modMask', k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask'}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask', button1), \w -> focus w >> mouseMoveWindow w),
      -- mod-button2, Raise the window to the top of the stack
      ((modMask', button2), \w -> focus w >> windows W.swapMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ((modMask', button3), \w -> focus w >> mouseResizeWindow w)
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q. Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
myStartupHook :: X ()
myStartupHook = pure ()

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up. --
-- main = xmonad =<< xmobar defaultConfig { terminal = "urxvtc" }

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $
    docks $
      ewmhFullscreen . ewmh $
        defaults
          { manageHook = manageDocks <+> manageHook def,
            layoutHook = defaultLayouts,
            logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmproc,
                    ppCurrent = xmobarColor "#f7f9fb" "",
                    ppHidden = xmobarColor "#657b83" "",
                    ppWsSep = xmobarColor "#002b36" "" " ",
                    ppSep = xmobarColor "#002b36" "" "    ",
                    ppUrgent = xmobarColor "#bf8b56" "",
                    ppTitle = xmobarColor "#f7f9fb" "",
                    ppLayout = const "" -- to disable the layout info on xmobar
                  }
                >> updatePointer (0.5, 0.5) (0, 0)
          }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults =
  def
    { terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      keys = myKeys,
      mouseBindings = myMouseBindings,
      manageHook = myManageHook,
      startupHook = myStartupHook
    }
