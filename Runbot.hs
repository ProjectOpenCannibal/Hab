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

runbot :: IO ()
runbot = bracket connect disconnect loop
	where
	  disconnect = hClose . socket
	  --Need to wrap my head around a proper IOException catch for this
	  loop st = runReaderT run st

-- Join our primary channel and initialize our listener
run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick++" 0 * :POC Bot")
	write "JOIN" chan
	asks socket >>= listen
