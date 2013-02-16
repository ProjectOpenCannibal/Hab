module Main () where

import qualified Control.Exception as E
import Control.Monad.State
import System.IO

-- Local modules
import Addons.IRC.Common
import Lib.IRC.Eval.HabCommands
import Lib.IRC.Net.HabIRCNet

runbot :: IO ()
runbot = E.bracket connect disconnect loop >> return ()
  where
    disconnect = hClose . socket
    loop st = E.catch (runStateT run st) (\e -> const (return ((), st)) (e :: E.IOException))

-- Join our primary channel and initialize our listener
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :"++realname)
    joinchan chan
    joinAddonChans
    gets socket >>= listen
