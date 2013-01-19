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
	-- Need to rewrite this to pass sender, channel (received from) and content
	-- to eval instead of simply the final content after the second colon
	-- see eval for more information on how I would 'like' to handle this
	clean	= drop 1 . dropWhile (/= ':') . drop 1
	ping x	= "PING :" `isPrefixOf` x
	pong x	= write "PONG" (':' : drop 6 x)
