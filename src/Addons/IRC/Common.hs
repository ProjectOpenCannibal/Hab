module Addons.IRC.Common (
    joinAddonChans
    , listAddons
    , listAddonsAdmin
    , evalAddons
    , evalAddonsAdmin
    , usageAddons
    ) where

import Addons.IRC.KindleFire.KFCommon
import Lib.IRC.Net.Socket

joinAddonChans :: Net ()
joinAddonChans = do
    --joinchan kf1
    --joinchan kf2
    return ()

listAddons :: String -> Net ()
listAddons user = do
    listKindleAddons user

listAddonsAdmin :: String -> Net ()
listAddonsAdmin user = do
    return ()

evalAddons :: String -> String -> String -> Net ()
evalAddons user origin content = do
    evalKindleAddons user origin content

evalAddonsAdmin :: String -> String -> String -> Net ()
evalAddonsAdmin user usrreal content = do
    return ()

usageAddons :: String -> String -> Net ()
usageAddons user content = do
    return ()
