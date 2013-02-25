module Addons.IRC.KindleFire.KF2 (
    listKF2Cmds     -- String -> Net ()
    , evalKF2Addons -- String -> String -> String -> Net ()
    , kf2           -- String
    ) where

import Lib.IRC.Socket

kf2 = "#kf2-dev"

kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
kf2rootlink = "http://forum.xda-developers.com/showthread.php?t=2075959"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"

listKF2Cmds :: String -> Net ()
listKF2Cmds origin = privmsg origin "!moorom, !oneclick, !root and !rts"

evalKF2Addons :: String -> String -> String -> Net ()
evalKF2Addons user origin content = 
    case content of
        -- (keep in alpha)
        "!moorom"   -> privmsg origin moorom
        "!oneclick" -> privmsg origin oneclick
        "!root"     -> privmsg origin kf2rootlink
        "!rts"      -> privmsg origin kf2rts
        otherwise   -> return ()
