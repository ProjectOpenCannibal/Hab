module Addons.IRC.Common (
    listAddons
    , evalAddons
    ) where

import Addons.IRC.KindleFire.KFCommon
import Lib.IRC.Net.Socket

listAddons :: String -> Net ()
listAddons user = do
    listKindleAddons user

evalAddons :: String -> String -> String -> Net ()
evalAddons user origin content = do
    evalKindleAddons user origin content
