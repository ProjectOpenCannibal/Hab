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
    -- channel or nick as #example : <message>
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
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)-- Single arg commands (keep in alpha)

eval "FMKilo-d2usc" _ x
    -- it's the variables that actually control who is being interpreted. y = FMKilo-d2usc... and so on. you can also decide who gets what privs this way.  
    ---- reply to FMKilo : Ing.
    -- This is not actually the case...
    -- Listen.hs is passing 3 strings through to eval, represented by our 3
    -- items directly following eval.
    --
    -- This first is the Nickname of whomever sent the message (we need a better
    -- way to authenticate our admins)
    -- The second I haven't gotten to work properly and this is the channel
    -- the message came in on, such as private message or #projectopencannibal
    -- the third variable (being assigned to X by the eval command is the
    -- content of the message itself; this is not ideal and needs to be parsed
    -- differently do to errors like the need for a colon in msg)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

-- Respond to everyone...
-- Post source / commit history link
-- use privmsg (used by ID)for this purpose (for now); will cause message to go
-- to primary channel only. Source is also in Realname so this shouldn't be too
-- big of an issue as is
eval _ _ "!source" = privmsg "https://github.com/ProjectOpenCannibal/CannibalismBot/commits/"

--eval "NickServ" _ ab
    -- I was trying to get it to auto authenticate for my bot... No such luck... Perhaps there is another way. 
    -- | "This nickname is registered." `isPrefixOf` ab = write "PRIVMSG NickServ identify <passwd>" (drop 5 ab)
    ---- reply to FMKilo : Ing.
    -- The first reason this doesn't work is 'This nickname is registered' is
    -- not the prefix of ab; it is the actual content of ab.
    -- the second reason is you're sending the same message back at the end of
    -- your identify command with the first 5 characters dropped off the string
    --
    -- see my broken identify command listen in Runbot.hs for a possible method
    -- (just need to fix my usage of readFile)

eval _ _ _ = return () -- ignore everything else
