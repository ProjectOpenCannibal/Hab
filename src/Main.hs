module Main (
    run
    , runbot
    ) where

import qualified Control.Exception as E
import Control.Monad.State
import System.IO

-- Local modules
import Lib.Net.Listen
import Lib.Net.Socket
import Lib.Write

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
    write "JOIN" chan
    gets socket >>= listen
