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
eval "IngCr3at1on" _ "!deftopic" = write ("PRIVMSG chanserv :topic "++chan) deftopic

-- Single arg commands (keep in alpha)
eval "IngCr3at1on" _ x
    -- remember this is directed to the primary channel only; use msg for
    -- everything else.
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)

-- In lou of a proper list, overwrite eval per FMKilo's recommendation.
eval "FMKilo" _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

eval "FMKilo-d2usc" _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

-- Modify identify command from FMKilo bot (works but being left out do to not
-- having a proper way to grab the PW from text and I'm not going to remember
-- to remove it from the source everytime I want to push a change)
-- To utilize uncomment and add a password variable towards the top of the file
-- Please note this will change.
--eval "NickServ" _ x
--    | "This nickname is registered." `isPrefixOf` x = write "PRIVMSG nickserv :identify" password

-- Respond to everyone...
-- Post source / commit history link
-- use privmsg (used by ID)for this purpose (for now); will cause message to go
-- to primary channel only. Source is also in Realname so this shouldn't be too
-- big of an issue as is
eval _ _ "!source" = privmsg source

eval _ _ _ = return () -- ignore everything else
