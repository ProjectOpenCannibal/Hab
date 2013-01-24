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
-- I'm unable to make this apply to a list of users instead of the single option
eval "IngCr3at1on" _ "!quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Single arg commands
eval "IngCr3at1on" _ x
-- I'm unable to make this apply to a list of users instead of the single option
-- for now the only commands included are ones we would want to limit to admins
-- anyway since I'm the only admin (online), the above works (I will want to
-- change it as soon as I figure out how though).
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

eval _ _ _ = return () -- ignore everything else
