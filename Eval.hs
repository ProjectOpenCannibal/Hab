module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!part " `isPrefixOf` x = write "PART" (drop 5 x)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
eval _ = return () -- ignore everything else
