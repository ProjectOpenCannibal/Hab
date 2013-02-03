module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Define any strings that we use on a regular basis (links etc)
-- should really go in a resource file rather than hard coded into the source
clilink = "http://terokarvinen.com/command_line.html"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

-- Evaluate a command
--
-- SndNick -> Origin -> Msgtype -> content (command)
eval :: String -> String -> String -> String -> Net ()
-- Non-argumental commands (keep in alpha)
-- I'm unable to make this apply to a list of users instead of the single option
eval "IngCr3at1on" _ _ "!deftopic" = write ("TOPIC "++chan) (" :"++deftopic)
eval "IngCr3at1on" _ _ "!opme" = write "MODE" (chan++" +o IngCr3at1on")
eval "IngCr3at1on" _ _ "!quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Single arg commands (keep in alpha)
eval "IngCr3at1on" _ _ x
    -- remember this is directed to the primary channel only; use msg for
    -- everything else.
    | "!deop " `isPrefixOf` x = write ("MODE "++chan++" -o") (drop 6 x)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    -- | "!msg" `isPrefixOf` x = write "PRIVMSG" ((getChan x)++":"++(getMsg x))
    | "!op " `isPrefixOf` x = write ("MODE "++chan++" +o") (drop 4 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (" :"++drop 7 x)

-- In lou of a proper list, overwrite eval per FMKilo's recommendation.
eval "FMKilo" _ _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

eval "FMKilo-d2usc" _ _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)

-- Respond to everyone...

-- resopond to the channel we received the message on (this doesn't work w/
-- private messages; yet, I have an idea just lazy)
eval _ y _ x
    | "!adb" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!cli" `isInfixOf` x = write "PRIVMSG" (y++" :"++clilink)
    | "!fastboot" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!source" `isInfixOf` x = write "PRIVMSG" (y++" :"++source)
    | "!udev" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)

eval _ _ _ _ = return () -- ignore everything else
