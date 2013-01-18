module Listen (listen) where

import Data.List
import Network
import System.IO
import System.Exit

-- Local modules
import Eval
import Socket
import Write

-- Listen to the socket and respond
listen :: Handle -> Net ()
listen h = forever $ do
	s <- init `fmap` io (hGetLine h)
	io (putStrLn s)
	if ping s then pong s else eval (clean s)
  where
	forever a = a >> forever a
	clean	= drop 1 . dropWhile (/= ':') . drop 1
	ping x	= "PING :" `isPrefixOf` x
	pong x	= write "PONG" (':' : drop 6 x)
