module Eval (eval, evalmode, mayberejoin) where

import Data.List
import qualified Data.Text as T
import System.Exit

-- Local modules
import Socket
import Write

-- Define any strings that we use on a regular basis (links etc)
-- should really go in a resource file rather than hard coded into the source
clilink = "http://terokarvinen.com/command_line.html"
kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"
kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

-- Define admins and gods (gods have quit and op assignment controls)
gods = ["IngCr3at1on"]
admins = ["IngCr3at1on", "FMKilo", "Hashcode", "iytrix"]

-- Evaluate a command
--
-- SndNick -> Origin -> Msgtype -> content (command)
-- (we drop type for now cause nothing uses it)
eval :: String -> String -> String -> String -> Net ()
eval u o _ c = do
    if isPriv o
        then do
            if isGod u
                then do
                    evalgodcmd u c
                    evaladcmd u c
                    evalprivcmd u c
                else if isAdmin u
                    then do
                    evaladcmd u c
                    evalprivcmd u c
                else evalprivcmd u c
        else evalchancmd u o c
  where
    isAdmin x = x `elem` admins
    isGod x = x `elem` gods
    isPriv x = "Hab" == x

-- Evaluate god commands
--
-- SndNick -> content (command)
evalgodcmd :: String -> String -> Net ()
evalgodcmd u c
    | "~deop " `isPrefixOf` c = write ("MODE "++chan++" -o") (drop 6 c)
    | "~op " `isPrefixOf` c = write ("MODE "++chan++" +o") (drop 4 c)
    | "~opme" == c = write "MODE" (chan++" +o "++u)
    | "~quit" == c = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
    | otherwise = return ()

-- process admin evaluation in the same way as gods
evaladcmd :: String -> String -> Net ()
evaladcmd u c
    | "~commands" `isInfixOf` c = listadcom u
    | "~deftopic" `isPrefixOf` c = write ("TOPIC"++chan) (" :"++deftopic)
    | "~id " `isPrefixOf` c = privmsg (drop 4 c)
    | "~join " `isPrefixOf` c = write "JOIN" (drop 6 c)
    | "~kick " `isPrefixOf` c = write "KICK" (drop 6 c)
    | "~me " `isPrefixOf` c = privmsg ("\001ACTION "++(drop 4 c)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` c = write "PRIVMSG" (drop 5 c)
    | "~part " `isPrefixOf` c = write "PART" (drop 6 c)
    | "~topic " `isPrefixOf` c = write ("TOPIC "++chan) (" :"++drop 7 c)
    | otherwise = return ()
 
-- Evaluate commands sent as private messages
--
-- SndNick -> content (command)
evalprivcmd :: String -> String -> Net ()
evalprivcmd u c
    | "!cli" `isInfixOf` c = write "PRIVMSG" (u++" :"++clilink)
    | "!commands" `isInfixOf` c = listcom u
    | "!source" `isInfixOf` c = write "PRIVMSG" (u++" :"++source)
    | otherwise = return ()
 
-- Evaluate in channel commands
--
-- Sndnick (user) -> Origin -> content (command)
evalchancmd :: String -> String -> String -> Net ()
evalchancmd u o c
    | "!cli" == c = write "PRIVMSG" (o++" :"++clilink)
    | "!commands" == c = listcom o
    | "!source" `isInfixOf` c = write "PRIVMSG" (o++" :"++source)
evalchancmd _ o c = do
    if kf1talk o
        then do
            if guide c
                then write "PRIVMSG" (o++" :"++kf1guide)
                else if udev c
                    then write "PRIVMSG" (o++" :"++udevsetup)
                else return ()
        else if kf2talk o
            then do
                if moo c
                    then write "PRIVMSG" (o++" :"++moorom)
                    else if onclick c
                        then write "PRIVMSG" (o++" :"++oneclick)
                    else if retstc c
                        then write "PRIVMSG" (o++" :"++kf2rts)
                    else if udev c
                        then write "PRIVMSG" (o++" :"++udevsetup)
                    else return ()
        else return ()
  where
    guide x = x == "!guide"
    kf1talk x = x == "#kindlefire-dev"
    kf2talk x = x == "#kf2-dev"
    moo x = x == "!moorom"
    onclick x = x == "!oneclick"
    retstc x = x == "!rts"
    udev x = x == "!udev"
evalchancmd _ _ _ = return ()

-- Evaluate a MODE change
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode c "-o" "Hab" = write "PRIVMSG" ("chanserv :op "++c++" Hab")
evalmode _ _ _ = return ()

-- Check who was kicked and if it was the bot, rejoin the channel in question
mayberejoin :: String -> Net ()
mayberejoin s = do
    if check s
        then write "JOIN" (origin s)
        else return ()
  where
    check x = "Hab" == (whois s)
    origin = (!! 2) . words
    whois = (!! 3) . words
