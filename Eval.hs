module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- SndNick -> SndFrom (channel/privchat etc) -> content (command)
eval :: String -> String -> String -> Net ()
-- Non-argumental commands (keep in alpha)
eval _ _ "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
-- Single arg commands
eval "IngCr3at1on" _ x | "!id " `isPrefixOf` x = privmsg (drop 4 x) 
eval "IngCr3at1on" _ x | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
eval "IngCr3at1on" _ x | "!part " `isPrefixOf` x = write "PART" (drop 5 x)
eval _ _ _ = return () -- ignore everything else
