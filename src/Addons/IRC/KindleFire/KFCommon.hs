module Addons.IRC.KindleFire.KFCommon (
    listKindleAddons   -- String -> Net ()
    , evalKindleAddons -- String -> String -> String -> Net ()
    -- export our channel references
    , kf1              -- String
    , kf2              -- String
    ) where

import Addons.IRC.KindleFire.KF1
import Addons.IRC.KindleFire.KF2
import Lib.IRC.Socket

chanspeccmd = "The following commands are specific to this channel"

listKindleAddons :: String -> Net ()
listKindleAddons user
    | kf1 == user = do
        privmsg user chanspeccmd
        listKF1Cmds user
    | kf2 == user = do
        privmsg user chanspeccmd
        listKF2Cmds user
    | otherwise = return ()

evalKindleAddons :: String -> String -> String -> Net ()
evalKindleAddons user origin content
    | kf1 == origin = evalKF1Addons user origin content
    | kf2 == origin = evalKF2Addons user origin content
    | otherwise = return ()
