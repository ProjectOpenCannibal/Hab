module Runbot (runbot, run) where

import Control.Exception
import Control.Monad.Reader
import Network
import System.IO
import Text.Printf

--Local modules
import Listen
import Socket
import Write

runbot :: IO ()
runbot = bracket connect disconnect loop
	where
	  disconnect	= hClose . socket
	  --Old.Exception isn't loading in Arch so we're leaving the catch out of
	  --this for now.
	  loop st		= runReaderT run st

-- Join a channel and initialize our listener
run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick++" 0 * :POC Bot")
	write "JOIN" chan
	asks socket >>= listen
