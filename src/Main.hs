{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (void)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Time
import GHC.Generics
import Options.Generic
import System.Exit
import System.FilePath ((<.>), (</>))
import System.IO
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Pass (passGeneratePrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.Run (runProcessWithInput, spawnPipe)

--------------------------------------------------------------------------------

enableLaptopMonitor :: X ()
enableLaptopMonitor = spawn "xrandr --output eDP-1  --auto"

disableLaptopMonitor :: X ()
disableLaptopMonitor = spawn "xrandr --output eDP-1  --off"

enableExternalMonitor :: X ()
enableExternalMonitor = spawn "xrandr --output HDMI-2 --auto"

disableExternalMonitor :: X ()
disableExternalMonitor = spawn "xrandr --output HDMI-2 --off"

-- Workspaces

myWorkspaces :: [String]
myWorkspaces = show @Int <$> [1 .. 9]

-- Layouts
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

-- Colors and borders

myNormalBorderColor :: String
myNormalBorderColor = "#405c79"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bf8b56"

myBorderWidth :: Dimension
myBorderWidth = 2

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { position = Top,
      font = "xft:AnonymicePro Nerd Font:pixelsize=12",
      bgColor = "#f7f9fb",
      fgColor = "#002b36",
      bgHLight = "#002b36",
      fgHLight = "#f7f9fb",
      height = 16,
      complCaseSensitivity = CaseInSensitive
    }

data GreenclipPrompt = GreenclipPrompt

instance XPrompt GreenclipPrompt where
  showXPrompt _ = "Clipboard: "

greenclipPrompt :: XPConfig -> X ()
greenclipPrompt c = do
  outputs <- lines <$> runProcessWithInput "greenclip" ["print"] ""
  mkXPrompt GreenclipPrompt c (mkCompletion outputs) copyToClipboard
  where
    mkCompletion = mkComplFunFromList c
    copyToClipboard str = void $ runProcessWithInput "xsel" ["-i", "-b"] str

-- Key bindings

myModMask :: KeyMask
myModMask = mod4Mask

xf86AudioLowerVolume :: KeySym
xf86AudioLowerVolume = 0x1008ff11

xf86AudioMute :: KeySym
xf86AudioMute = 0x1008ff12

xf86AudioRaiseVolume :: KeySym
xf86AudioRaiseVolume = 0x1008ff13

xf86AudioNext :: KeySym
xf86AudioNext = 0x1008ff17

xf86AudioPrev :: KeySym
xf86AudioPrev = 0x1008ff16

xf86AudioPlay :: KeySym
xf86AudioPlay = 0x1008ff14

xf86AudioStop :: KeySym
xf86AudioStop = 0x1008ff15

takeScreenshot :: FilePath -> IO ()
takeScreenshot screenshotDir = do
  now <- getCurrentTime
  let format = "%Y%m%H%M%S"
  let formattedDate = formatTime defaultTimeLocale format now
  let filename = screenshotDir </> "screenshot-" <> formattedDate <.> "png"
  let maimCmd = "maim -sulc 0.9,0.6,0.3,0.4 " <> filename
  let notifyCmd =
        unwords
          [ "notify-send --action='xdg-open",
            filename <> "'",
            "screenshot",
            filename
          ]
  spawn $ unwords [maimCmd, "&&", notifyCmd]

myKeys :: FilePath -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys screenshotDir conf@(XConfig {XMonad.modMask = modMask'}) =
  M.fromList $
    [ ((modMask' .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      -- Screenshot
      ((0, 0x1008ff81), liftIO $ takeScreenshot screenshotDir),
      ((modMask', xK_p), liftIO $ takeScreenshot screenshotDir),
      ((modMask', xK_r), runOrRaisePrompt myPromptConfig),
      -- Multimedia keys
      ((0, xf86AudioLowerVolume), spawn "pulsemixer --change-volume -5 --max-volume 100"),
      ((0, xf86AudioRaiseVolume), spawn "pulsemixer --change-volume +5 --max-volume 100"),
      ((0, xf86AudioMute), spawn "pulsemixer --set-volume 0 --max-volume 100"),
      ((shiftMask, xf86AudioMute), spawn "pulsemixer --set-volume 100 --max-volume 100"),
      ((0, xf86AudioNext), spawn "playerctl next"),
      ((0, xf86AudioPrev), spawn "playerctl previous"),
      ((0, xf86AudioPlay), spawn "playerctl play-pause"),
      ((0, xf86AudioStop), spawn "playerctl stop"),
      ( (modMask', xK_c),
        greenclipPrompt $ myPromptConfig {searchPredicate = isInfixOf}
      ),
      ((modMask' .|. shiftMask, xK_p), XMonad.Prompt.Pass.passGeneratePrompt myPromptConfig),
      ((modMask', xK_e), enableExternalMonitor),
      ((modMask' .|. shiftMask, xK_e), disableExternalMonitor),
      ((modMask', xK_i), enableLaptopMonitor),
      ((modMask' .|. shiftMask, xK_i), disableLaptopMonitor),
      ((modMask', xK_f), sendMessage ToggleStruts),
      -- Lock and suspend/hibernate
      ((modMask' .|. shiftMask, xK_l), spawn "dm-tool lock"),
      ((modMask' .|. shiftMask, xK_s), spawn "systemctl suspend"),
      ((modMask' .|. shiftMask, xK_h), spawn "systemctl hybrid-sleep"),
      -- Standard xmonad key bindings
      ((modMask' .|. shiftMask, xK_c), kill),
      ((modMask', xK_space), sendMessage NextLayout),
      ((modMask' .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      ((modMask', xK_n), refresh),
      ((modMask', xK_Tab), windows W.focusDown),
      ((modMask' .|. shiftMask, xK_Tab), windows W.focusUp),
      ((modMask', xK_j), windows W.focusDown),
      ((modMask', xK_k), windows W.focusUp),
      ((modMask', xK_m), windows W.focusMaster),
      ((modMask', xK_Return), windows W.swapMaster),
      ((modMask' .|. shiftMask, xK_j), windows W.swapDown),
      ((modMask' .|. shiftMask, xK_k), windows W.swapUp),
      ((modMask', xK_h), sendMessage Shrink),
      ((modMask', xK_l), sendMessage Expand),
      ((modMask', xK_t), withFocused $ windows . W.sink),
      ((modMask', xK_comma), sendMessage (IncMasterN 1)),
      ((modMask', xK_period), sendMessage (IncMasterN (-1))),
      ((modMask' .|. shiftMask, xK_q), io exitSuccess),
      ((modMask', xK_grave), toggleWS),
      ((modMask', xK_q), restart "xmonad" True)
    ]
      ++ [ ((m .|. modMask', k), windows $ f i)
         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
           (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]

-- Mouse bindings

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask'}) =
  M.fromList
    [ ((modMask', button1), \w -> focus w >> mouseMoveWindow w),
      ((modMask', button2), \w -> focus w >> windows W.swapMaster),
      ((modMask', button3), \w -> focus w >> mouseResizeWindow w)
    ]

-- Startup hook

myStartupHook :: String -> X ()
myStartupHook wallpaperPath = spawn $ "xwallpaper --zoom " <> wallpaperPath

--------------------------------------------------------------------------------

data CLIOptions = CLIOptions
  { xmobarConfig :: String,
    wallpaper :: String,
    term :: String,
    screenshotDir :: String
  }
  deriving (Generic)

instance ParseRecord CLIOptions where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  CLIOptions {..} <- getRecord "xmonad-damianfral"
  xmproc <- spawnPipe $ "xmobar " <> xmobarConfig
  let xConfig =
        docks . ewmhFullscreen . ewmh $
          mkXConfig xmproc wallpaper term screenshotDir
  xmonad xConfig
  where
    mkXConfig xmproc wallpaper term screenshotDir =
      def
        { terminal = term,
          focusFollowsMouse = myFocusFollowsMouse,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          keys = myKeys screenshotDir,
          mouseBindings = myMouseBindings,
          startupHook = myStartupHook wallpaper,
          manageHook = manageDocks <+> manageHook def,
          layoutHook = defaultLayouts,
          logHook = do
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppCurrent = xmobarColor "#f7f9fb" "",
                  ppHidden = xmobarColor "#657b83" "",
                  ppWsSep = xmobarColor "#002b36" "" " ",
                  ppSep = xmobarColor "#002b36" "" "    ",
                  ppUrgent = xmobarColor "#bf8b56" "",
                  ppTitle = xmobarColor "#f7f9fb" "",
                  ppLayout = const ""
                }
            updatePointer (0.5, 0.5) (0, 0)
        }
