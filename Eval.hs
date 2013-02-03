module Eval (eval, evalmode, rejoin) where

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

-- Evaluate 'God' commands first (followed by other admin than private messages)
-- I'm unable to make this apply to a list of users instead of the single option
eval "IngCr3at1on" "Hab" _ "~deftopic" = write ("TOPIC "++chan) (" :"++deftopic)
eval "IngCr3at1on" "Hab" _ "~opme" = write "MODE" (chan++" +o IngCr3at1on")
eval "IngCr3at1on" "Hab" _ "~quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

eval "IngCr3at1on" "Hab" _ x
    -- remember this is directed to the primary channel only; use msg for
    -- everything else.
    | "~commands" `isInfixOf` x = listadcom "IngCr3at1on"
    | "~deop " `isPrefixOf` x = write ("MODE "++chan++" -o") (drop 6 x)
    | "~id " `isPrefixOf` x = privmsg (drop 4 x)
    | "~join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "~kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "~me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "~op " `isPrefixOf` x = write ("MODE "++chan++" +o") (drop 4 x)
    | "~part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "~topic " `isPrefixOf` x = write ("TOPIC "++chan) (" :"++drop 7 x)

-- In lou of a proper list, overwrite eval per FMKilo's recommendation.
eval "FMKilo" "Hab" _ x
    | "~commands" `isInfixOf` x = listadcom "FMKilo"
    | "~id " `isPrefixOf` x = privmsg (drop 4 x)
    | "~join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "~kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "~me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "~msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "~part " `isPrefixOf` x = write "PART" (drop 6 x)

eval "FMKilo-d2usc" "Hab" _ x
    | "~commands" `isInfixOf` x = listadcom "FMKilo-d2usc"
    | "~id " `isPrefixOf` x = privmsg (drop 4 x)
    | "~join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "~kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "~me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "~msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "~part " `isPrefixOf` x = write "PART" (drop 6 x)

-- Evaluate private messages (only neccessary if a response is required)
eval y "Hab" _ x
    | "!adb" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!cli" `isInfixOf` x = write "PRIVMSG" (y++" :"++clilink)
    | "!commands" `isInfixOf` x = listcom y
    | "!fastboot" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!source" `isInfixOf` x = write "PRIVMSG" (y++" :"++source)
    | "!udev" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)

-- Respond to everyone...

-- resopond to the channel we received the message on (this doesn't work w/
-- private messages; yet, I have an idea just lazy)
eval _ y _ x
    | "!adb" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!cli" `isInfixOf` x = write "PRIVMSG" (y++" :"++clilink)
    | "!commands" `isInfixOf` x = listcom y
    | "!fastboot" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)
    | "!source" `isInfixOf` x = write "PRIVMSG" (y++" :"++source)
    | "!udev" `isInfixOf` x = write "PRIVMSG" (y++" :"++udevsetup)

eval _ _ _ _ = return () -- ignore everything else

-- Evaluate a MODE change
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode c "-o" "Hab" = write "PRIVMSG" ("chanserv :op "++c++" Hab")
evalmode _ _ _ = return ()

rejoin :: String -> Net ()
rejoin x = write "JOIN" x
