module Addons.IRC.KindleFire.Common (
    listKindleAddons
    , evalKindleAddons
    ) where

import Addons.IRC.KindleFire.KF1
import Addons.IRC.KindleFire.KF2
import Lib.Net.Socket
import Lib.Write

chanspeccmd = "The following commands are specific to this channel"

listKindleAddons :: String -> Net ()
listKindleAddons user = do
    if user == kf1
        then do
            write ("PRIVMSG "++user++" :") chanspeccmd
            listKF1Cmds user
        else if user == kf2
            then do
                write ("PRIVMSG "++user++" :") chanspeccmd
                listKF2Cmds user
        else return ()

evalKindleAddons :: String -> String -> String -> Net ()
evalKindleAddons user origin content = do
    if origin == kf1
        then evalKF1Addons user origin content
        else if origin == kf2
            then evalKF2Addons user origin content
        else return ()
