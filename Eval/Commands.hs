module Eval.Commands (
    -- Command listing
    listadcom,
    listcom,
    -- Command evaluation
    evaladcmd,
    evalchancmd,
    evalgodcmd,
    evalprivcmd
    ) where

import Data.List
import qualified Data.Text as T
import System.Exit

-- Local modules
import Socket
import Write

---- Resources

-- These will all be moved into a resource file
--
-- Common strings used throughout the file
chanspeccmd = "The following commands are specific to this channel"

-- common links (available in all channels)
clilink = "http://terokarvinen.com/command_line.html"

-- links available to the kindlefire-dev channel only
kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"

-- links available to the kf2-dev channel only
kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
kf2rootlink = "http://forum.xda-developers.com/showthread.php?t=2075959"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

---- Command listing

-- List non-admin commands
listcom :: String -> Net ()
listcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported commands are as follows:")
    write ("PRIVMSG "++s++" :") ("!commands, !cli and !source")
    if kf1talk s
        then do
            write ("PRIVMSG "++s++" :") chanspeccmd
            write ("PRIVMSG "++s++" :") ("!guide and !udev")
        else if kf2talk s
            then do
                write ("PRIVMSG "++s++" :") chanspeccmd
                write ("PRIVMSG "++s++" :") ("!moorom, !oneclick, !rts and !udev")
        else return ()
  where
    kf1talk x = x == "#kindlefire-dev"
    kf2talk x = x == "#kf2-dev"

-- List admin commands
listadcom :: String -> Net ()
listadcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported admin commands are as follows:")
    write ("PRIVMSG "++s++" :") ("~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part")
    write ("PRIVMSG "++s++" :") ("Please note ~me may be relocated")

---- Command evaluation

-- Process god commands
--
-- SndNick -> content (command)
evalgodcmd :: String -> String -> Net ()
evalgodcmd u c
    | "~quit" == c = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
    | otherwise = return ()

-- Process admin evaluation in the same way as gods
evaladcmd :: String -> String -> Net ()
evaladcmd u c
    | "~commands" `isInfixOf` c = listadcom u
    | "~deftopic" `isPrefixOf` c = write ("TOPIC"++chan) (" :"++deftopic)
    | "~deop " `isPrefixOf` c = write ("MODE "++chan++" -o") (drop 6 c)
    | "~id " `isPrefixOf` c = privmsg (drop 4 c)
    | "~join " `isPrefixOf` c = write "JOIN" (drop 6 c)
    | "~kick " `isPrefixOf` c = write "KICK" (drop 6 c)
    | "~me " `isPrefixOf` c = privmsg ("\001ACTION "++(drop 4 c)++"\001")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` c = write "PRIVMSG" (drop 5 c)
    | "~op " `isPrefixOf` c = write ("MODE "++chan++" +o") (drop 4 c)
    | "~opme" == c = write "MODE" (chan++" +o "++u)
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
    | otherwise = do
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
                            else if root c
                                then write "PRIVMSG" (o++" :"++kf2rootlink)
                            else if udev c
                                 then write "PRIVMSG" (o++" :"++udevsetup)
                            else return ()
            else return ()
  where
    -- Channel calls (keep in alpha)
    kf1talk x = x == "#kindlefire-dev"
    kf2talk x = x == "#kf2-dev"
    -- Command calls
    guide x = x == "!guide"
    moo x = x == "!moorom"
    onclick x = x == "!oneclick"
    retstc x = x == "!rts"
    root x = x == "!root"
    udev x = x == "!udev"
