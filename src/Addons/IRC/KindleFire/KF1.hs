module Addons.IRC.KindleFire.KF1 (
    listKF1Cmds
    , evalKF1Addons
    , kf1
    ) where

import Lib.Net.Socket
import Lib.Write

kf1 = "#kindlefire-dev"

kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

listKF1Cmds :: String -> Net ()
listKF1Cmds user = write ("PRIVMSG "++user++" :") ("!guide and !udev")

evalKF1Addons :: String -> String -> String -> Net ()
evalKF1Addons user origin content = do
    case content of
        -- (keep in alpha)
        "!guide" -> write "PRIVMSG" (origin++" :"++kf1guide)
        "!udev"  -> write "PRIVMSG" (origin++" :"++udevsetup)
        _        -> return ()
