module Eval.Commands (
    -- Command listing
    listadcom
    , listcom
    -- Command evaluation
    , evaladcmd
    , evalchancmd
    , evalgodcmd
    , evalprivcmd
    -- Bot commands
    , regainnick
    ) where

import Data.List
import qualified Data.Text as T
import System.Exit

-- Local modules
import Eval.Users
import Socket
import Write

---- Resources

-- These will all be moved into a resource file
--
-- Common strings used throughout the file
chanspeccmd = "The following commands are specific to this channel"

-- common links (available in all channels)
clilink = "http://terokarvinen.com/command_line.html"

-- links common to all kindlefire devices
udevsetup = "http://forum.xda-developers.com/showthread.php?t=1475740"

-- links available to the kindlefire-dev channel only
kf1guide = "http://forum.xda-developers.com/showthread.php?t=1552547"

-- links available to the kf2-dev channel only
kf2rts = "http://forum.xda-developers.com/showthread.php?t=2035047"
kf2rootlink = "http://forum.xda-developers.com/showthread.php?t=2075959"
moorom = "http://forum.xda-developers.com/showthread.php?t=2105077"
oneclick = "http://forum.xda-developers.com/showthread.php?t=2106463"

---- Command listing

-- List non-admin commands
listcom :: String -> Net ()
listcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported commands are as follows:")
    write ("PRIVMSG "++s++" :") ("!commands, !cli and !source")
    -- List any channel specific commands
    case s of
        -- (keep in alpha)
        "#kindlefire-dev" -> do
                                 write ("PRIVMSG "++s++" :") chanspeccmd
                                 write ("PRIVMSG "++s++" :") ("!guide and !udev")
        "#kf2-dev"        -> do
                                 write ("PRIVMSG "++s++" :") chanspeccmd
                                 write ("PRIVMSG "++s++" :") ("!moorom, !oneclick, !rts and !udev")
        _                 -> return ()

-- List admin commands
listadcom :: String -> Net ()
listadcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported admin commands are as follows:")
    write ("PRIVMSG "++s++" :") ("~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part")
    write ("PRIVMSG "++s++" :") ("Please note ~me may be relocated")

---- Command evaluation

-- Process god commands
--
-- SndNick -> SndReal -> content (command)
evalgodcmd :: String -> String -> String -> Net ()
evalgodcmd u r c
    | "~quit" == c = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
    | otherwise = return ()

-- Process admin evaluation in the same way as gods
evaladcmd :: String -> String -> String -> Net ()
evaladcmd u r c
    | "~commands" == c = listadcom u
    | "~deftopic" == c = write ("TOPIC"++chan) (" :"++deftopic)
    -- | "~deop " `isPrefixOf` c = revop (drop 6 c)
    -- | "~deop" == c = write "PRIVMSG" (u++" :Usage '~deop <nick> <channel>'")
    | "~id " `isPrefixOf` c = privmsg (drop 4 c)
    | "~id" == c = do
        write "PRIVMSG" (u++" :Usage: '~id <msg>'")
        write "PRIVMSG" (u++" :directs message to primary channel.")
    | "~join " `isPrefixOf` c = write "JOIN" (drop 6 c)
    | "~join" == c = write "PRIVMSG" (u++" :Usage: '~join <channel>'")
    | "~kick " `isPrefixOf` c = write "KICK" (drop 6 c)
    | "~kick" == c = write "PRIVMSG" (u++" :Usage: '~kick <channel> <nick> :<message>'")
    -- | "~me " `isPrefixOf` c = action (drop 4 c)
    -- | "~me" == c = write "PRIVMSG" (u++" :Usage '~me <channel> <action>'")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` c = write "PRIVMSG" (drop 5 c)
    -- | "~op " `isPrefixOf` c = setop (drop 4 c)
    -- | "~op" == c = write "PRIVMSG" (u++" :Usage '~op <nick> <channel>'")
    | "~opme" == c = write "MODE" (chan++" +o "++u)
    | "~part " `isPrefixOf` c = write "PART" (drop 6 c)
    | "~part" == c = write "PRIVMSG" (u++" :Usage: '~part <channel>'")
    | "~topic " `isPrefixOf` c = write ("TOPIC "++chan) (" :"++drop 7 c)
    | "~topic" == c = do
        write "PRIVMSG" (u++" :Usage: '~topic <topic>'")
        write "PRIVMSG" (u++" :please note this applies to "++chan++" only.")
    -- | "~verify" == c = write "PRIVMSG" (u++" : Usage is 'verify <password>'")
    -- | "~verify " `isPrefixOf` c = verifyNick u r c
    | otherwise = return ()
 
-- Evaluate commands sent as private messages
--
-- SndNick -> content (command)
evalprivcmd :: String -> String -> Net ()
evalprivcmd u c
    | "!cli" == c = write "PRIVMSG" (u++" :"++clilink)
    | "!commands" == c = listcom u
    | "!source" == c = write "PRIVMSG" (u++" :"++source)
    | otherwise = return ()
 
-- Evaluate in channel commands
--
-- Sndnick (user) -> Origin -> content (command)
evalchancmd :: String -> String -> String -> Net ()
evalchancmd u o c
    | "!cli" == c = write "PRIVMSG" (o++" :"++clilink)
    | "!commands" == c = listcom o
    | "!source" == c = write "PRIVMSG" (o++" :"++source)
    -- Evaluate channel specific commands
    | otherwise = do
        case c of
            -- (keep in alpha)
            "!guide"    -> if kf1talk o
                               then write "PRIVMSG" (o++" :"++kf1guide)
                               else return ()
            "!moorom"   -> if kf2talk o
                               then write "PRIVMSG" (o++" :"++moorom)
                               else return ()
            "!oneclick" -> if kf2talk o
                               then write "PRIVMSG" (o++" :"++oneclick)
                               else return ()
            "!rts"      -> if kf2talk o
                               then write "PRIVMSG" (o++" :"++kf2rts)
                               else return ()
            "!root"     -> if kf2talk o
                               then write "PRIVMSG" (o++" :"++kf2rootlink)
                               else return ()
            "!udev"     -> if kf1talk o || kf2talk o
                               then write "PRIVMSG" (o++" :"++udevsetup)
                               else return ()
            _           -> return ()
        where
          -- Channel calls (keep in alpha)
          kf1talk x = x == "#kindlefire-dev"
          kf2talk x = x == "#kf2-dev"

-- Add complex commands here

-- These are  all broken; provides this error :
-- Exception: Prelude.(!!): index too large
--
-- Assign op privs to a user in any channel we have op privs in
--
-- content (command)
setop :: String -> Net ()
setop c = let {
    dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in write ("MODE "++(dest c)++" +o") (mode c)

-- Revoke op privs from a user
--
-- content (command)
revop :: String -> Net ()
revop c = let {
    dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in write ("MODE "++(dest c)++" -o") (mode c)

-- Perform an action
--
-- content (command)
action :: String -> Net ()
action c = let {
    dest = (!! 0) . words
    ; func = (!! 1) . words
    } in write ("PRIVMSG"++(dest c)++" :") ("\001ACTION "++(func c)++"\001")

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile ".password")
    write "NICK" "HaskellBot"
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "JOIN" chan
