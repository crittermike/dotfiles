import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
 
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mcrittenden/.xmobarrc"
    xmonad $ gnomeConfig { 
        manageHook = manageDocks <+> manageHook defaultConfig, 
        layoutHook = avoidStruts  $  layoutHook defaultConfig, 
        logHook = dynamicLogWithPP xmobarPP { 
            ppOutput = hPutStrLn xmproc, 
            ppTitle = xmobarColor "green" "" . shorten 50
        }
    }
