module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- Need to rewrite this and Listen to evalute a sender, channel, content/command
-- and argument instead of just the one string of content (listen cleans
-- everything off prior to the second colon as is; this will need to be changed)
--
-- evaluate sender if required (for commands such as quit, join, ban etc)
--
-- prior to evaluating sender check the command (do we care who asked? if not
-- respond regardless)
--
-- if the command in question requires a secondary argument other than the
-- final section of the string as used currently check it as well (an example
-- of this would be any sort of message option/implementation; would require a
-- destination of either channel or nickname prior to processing the final
-- string (message))
--
-- Store the channel so we know where to respond if a response is required
--

-- SndNick -> SndFrom (channel/privchat etc) -> content (command)
eval :: String -> String -> String -> Net ()
-- Non-argumental commands (keep in alpha)
eval _ _ "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
-- Single arg commands
eval "IngCr3at1on" _ x | "!id " `isPrefixOf` x = privmsg y (drop 4 x) 
eval "IngCr3at1on" _ x | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
eval "IngCr3at1on" _ x | "!part " `isPrefixOf` x = write "PART" (drop 5 x)
eval _ _ _ = return () -- ignore everything else
