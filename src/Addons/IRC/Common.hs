module Addons.IRC.Common (
    joinAddonChans
    , listAddons
    , listAddonsAdmin
    , evalAddons
    , evalAddonsAdmin
    ) where

import Addons.IRC.KindleFire.KFCommon
import Lib.IRC.Eval.HabCommands
import Lib.IRC.Net.Socket

joinAddonChans :: Net ()
joinAddonChans = do
    joinchan kf1
    joinchan kf2

listAddons :: String -> Net ()
listAddons user = do
    listKindleAddons user

listAddonsAdmin :: String -> Net ()
listAddonsAdmin user = do
    return ()

evalAddons :: String -> String -> String -> Net ()
evalAddons user origin content = do
    evalKindleAddons user origin content

evalAddonsAdmin :: String -> String -> Net ()
evalAddonsAdmin user content = do
    return ()
