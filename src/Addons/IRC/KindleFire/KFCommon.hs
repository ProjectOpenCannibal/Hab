module Addons.IRC.KindleFire.KFCommon (
    joinKFchans        -- Net ()
    , listKindleAddons -- String -> Net ()
    , evalKindleAddons -- String -> String -> String -> Net ()
    ) where

import Addons.IRC.KindleFire.KF1
import Addons.IRC.KindleFire.KF2
import Lib.IRC.Socket

factorycable = "http://forum.xda-developers.com/showthread.php?t=1550999"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

kindles = [ kf1, kf2 ]

isKindle :: String -> Bool
isKindle str = str `elem` kindles

joinKFchans :: Net ()
joinKFchans = do
    joinchan kf1
    joinchan kf2

{- List can be considered user or origin, being that these are channel specific
   we consider it origin. -}
listKindleAddons :: String -> Net ()
listKindleAddons origin =
    if isKindle origin
        then do
            privmsg origin "This channel also supports:"
            privmsg origin "!factory cable, !udev, "
            case origin of
                "#kindlefire-dev" -> listKF1Cmds origin
                "#kf2-dev"        -> listKF2Cmds origin
                -- This should never happen.
                otherwise         -> return ()
        else return ()

evalKindleAddons :: String -> String -> String -> Net ()
evalKindleAddons user origin content =
    if origin == kf1 || origin == kf2
        then case content of
            "!factory cable" -> privmsg origin factorycable
            "!udev"          -> privmsg origin udevsetup
            otherwise        -> if origin == kf1
                                    then evalKF1Addons user origin content
                                    else if origin == kf2
                                        then evalKF2Addons user origin content
                                    -- This should never happen.
                                    else return ()
        else return ()
