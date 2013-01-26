module Runbot (runbot, run) where

import Control.Exception
import Control.Monad.Reader
import Network
import System.IO
import Text.Printf
--Hide catch from w/in prelude as it's imported in Control.Exception
import Prelude hiding (catch)

--Local modules
import Listen
import Socket
import Write

-- This actually appears to work for setting a password variable from file but
-- I'm failing ot properly utilize it so leave it out for now
--password :: IO String
--password = readFile ".password"

runbot :: IO ()
runbot = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    -- To my understanding this should work, it does not...
    -- loop st = catch (runReaderT run st) (\e -> const (return ((), st)) (e :: IOException))
    loop st = runReaderT run st

-- Join our primary channel and initialize our listener
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :"++realname)
    write "JOIN" chan
    --identify
    asks socket >>= listen

-- This doesn't work because an IO String is not a String and I'm having issues
-- making Haskell not argue about my using it as such.
--identify :: Net ()
--identify = do
    --write "PRIVMSG" ("nickserv :identify "++password)
