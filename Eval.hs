module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- Need to rewrite this and Listen to evalute a sender, channel and content
-- instead of just the one string of content (listen cleans everything off
-- prior to the second colon as is; this will need to be changed).
eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!part " `isPrefixOf` x = write "PART" (drop 5 x)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
eval _ = return () -- ignore everything else
