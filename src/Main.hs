module Main (
    main
    , runbot
    ) where

import qualified Control.Exception as E
import Control.Monad.State
import System.IO

-- Local modules.
import Addons.IRC.IRCCommon -- Imported for our additional channels.
import Lib.IRC.HabCommands
import qualified Lib.IRC.HabIRC as IRC
--import qualified Lib.XMPP.HabXMPP as XMPP

runbot = main

main :: IO ()
main =
    let loop st = E.catch (runStateT run st) (\e -> const (return ((), st)) (e :: E.IOException))
        in E.bracket connect disconnect loop >> return ()

-- Join our primary channel and initialize our listener.
run :: IRC.Net ()
run = do
    IRC.write "NICK" IRC.nick
    IRC.write "USER" (IRC.nick++" 0 * :"++IRC.realname)
    IRC.joinchan IRC.chan
    joinAddonChans
    gets IRC.socket >>= IRC.listen

connect = do
    IRC.connect
    --XMPP.connect

disconnect = do
    hClose . IRC.socket
    --hClose . XMPP.socket
