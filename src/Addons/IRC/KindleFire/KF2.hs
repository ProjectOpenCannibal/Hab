module Addons.IRC.KindleFire.KF2 (
    listKF2Cmds
    , evalKF2Addons
    , kf2
    ) where

import Lib.Net.Socket
import Lib.Write

kf2 = "#kf2-dev"

-- links available to the kf2-dev channel only
kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
kf2rootlink = "http://forum.xda-developers.com/showthread.php?t=2075959"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

listKF2Cmds :: String -> Net ()
listKF2Cmds user = write ("PRIVMSG "++user++" :") ("!moorom, !oneclick, !rts and !udev")

evalKF2Addons :: String -> String -> String -> Net ()
evalKF2Addons user origin content = 
    case content of
        -- (keep in alpha)
        "!moorom"   -> privmsg origin moorom
        "!oneclick" -> privmsg origin oneclick
        "!rts"      -> privmsg origin kf2rts
        "!root"     -> privmsg origin kf2rootlink
        "!udev"     -> privmsg origin udevsetup
        _           -> return ()

