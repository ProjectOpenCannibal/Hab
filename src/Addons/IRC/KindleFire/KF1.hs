module Addons.IRC.KindleFire.KF1 (
    listKF1Cmds     -- String -> Net ()
    , evalKF1Addons -- String -> String -> String -> Net ()
    , kf1           -- String
    ) where

import Lib.IRC.Socket

kf1 = "#kindlefire-dev"

root = "http://forum.xda-developers.com/showthread.php?t=1568340"
kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"

listKF1Cmds :: String -> Net ()
listKF1Cmds origin = privmsg origin "!guide and !root"

evalKF1Addons :: String -> String -> String -> Net ()
evalKF1Addons user origin content =
    case content of
        -- (keep in alpha)
        "!guide"  -> privmsg origin kf1guide
        "!root"   -> privmsg origin root
        otherwise -> return ()
