module Addons.IRC.KindleFire.KF1 (
    listKF1Cmds
    , evalKF1Addons
    , kf1
    ) where

import Lib.IRC.Net.Socket

kf1 = "#kindlefire-dev"

kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

listKF1Cmds :: String -> Net ()
listKF1Cmds user = privmsg user "!guide and !udev"

evalKF1Addons :: String -> String -> String -> Net ()
evalKF1Addons user origin content =
    case content of
        -- (keep in alpha)
        "!guide"  -> privmsg origin kf1guide
        "!udev"   -> privmsg origin udevsetup
        otherwise -> return ()
