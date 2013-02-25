module Addons.IRC.IRCCommon (
    joinAddonChans    -- Net ()
    , listAddons      -- String -> Net ()
    , listAddonsAdmin -- String -> Net ()
    , evalAddons      -- String -> String -> String -> Net ()
    , evalAddonsAdmin -- String -> String -> String -> Net ()
    , usageAddons     -- String -> String -> Net ()
    ) where

import Addons.IRC.KindleFire.KFCommon
import Lib.IRC.Socket

joinAddonChans :: Net ()
joinAddonChans = do
    joinKFchans -- Join the Kindle Fire related channels

{- Non-admin commands are recognized in both private and in-channel messages
   therefore this can be considered to be user or origin. We call it user here
   as this is common to admin commands. -}
listAddons :: String -> Net ()
listAddons user = do
    listKindleAddons user -- Kindle Fire related commands : treat user as origin

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
