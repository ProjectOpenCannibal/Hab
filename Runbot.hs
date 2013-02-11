module Runbot (
    run
    , runbot
    ) where

import Control.Exception
import Control.Monad.State
import Network
import System.IO
import Text.Printf
--Hide catch from w/in prelude as it's imported in Control.Exception
import Prelude hiding (catch)

--Local modules
import Listen
import Socket
import Write

runbot :: IO ()
runbot = bracket connect disconnect loop >> return ()
  where
    disconnect = hClose . socket
    loop st = catch (runStateT run st) (\e -> const (return ((), st)) (e :: IOException))

-- Join our primary channel and initialize our listener
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :"++realname)
    write "JOIN" chan
    identify
    gets socket >>= listen

-- Auto identify on login (uses password stored in a local file '.password')
identify :: Net ()
identify = do
    password <- io (readFile ".password")
    write "PRIVMSG" ("nickserv :identify "++password)    
