module Lib.IRC.Eval.Commands (
    -- Command listing
    listadcom
    , listcom
    -- Command evaluation
    , evaladcmd
    , evalchancmd
    , evalgodcmd
    , evalprivcmd
    -- Bot commands
    , identify
    , regainnick
    ) where

--import Data.Either.Utils
--import Control.Monad.Error
import Data.List
import qualified Data.Text as T
import System.IO.Unsafe
import System.Exit

-- Hackage modules
--import Data.ConfigFile

-- Local modules
import Addons.IRC.Common
import Lib.IRC.Net.Socket
import Lib.IRC.Net.Write

---- Resources

-- These will all be moved into a resource file
--
-- common links (available in all channels)
clilink = "http://terokarvinen.com/command_line.html"

---- Command listing

-- List non-admin commands
listcom :: String -> Net ()
listcom user = do
    privmsg user "Currently supported commands are as follows:"
    privmsg user "!commands, !cli and !source"
    -- List any commands from our addons
    listAddons user

-- List admin commands
listadcom :: String -> Net ()
listadcom user = do
    privmsg user "Currently supported admin commands are as follows:"
    privmsg user "~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part"
    privmsg user "Please note ~me may be relocated"

---- Command evaluation

-- List all of our usage commands for easy reference
usage :: String -> String -> Net ()
usage user content =
    case content of
        "~deop"   -> privmsg user "Usage: '~deop <nick> <channel>'"
        "~id"     -> do
                         privmsg user "Usage: '~id <msg>'"
                         privmsg user ("directs message to "++chan++"only."
        "~join"   -> privmsg user "Usage: '~join <channel>'"
        "~kick"   -> privmsg user "Usage: '~kick <channel> <nick> :<message>'"
        "~me"     -> privmsg user "Usage: '~me <channel> <action>'"
        "~op"     -> privmsg user "Usage: '~op <nick> <channel>'"
        "~part"   -> privmsg user "Usage: '~part <channel>'"
        "~topic"  -> do
                         privmsg user "Usage: '~topic <topic>'"
                         privmsg user ("please note this applies to "++chan++" only.")
        otherwise -> return ()

-- Process god commands
evalgodcmd :: String -> String -> String -> Net ()
evalgodcmd user usrreal content
    | "~quit" == content = processquit
    | otherwise = return ()

-- Process admin evaluation in the same way as gods
evaladcmd :: String -> String -> String -> Net ()
evaladcmd user usrreal content
    | "~commands" == content = listadcom user
    | "~deop " `isPrefixOf` content = revop user (drop 6 content)
    | "~deop" == content = usage user content
    | "~id " `isPrefixOf` content = privmsg chan (drop 4 content)
    | "~id" == content = usage user content
    | "~join " `isPrefixOf` content = write "JOIN" (drop 6 content)
    | "~join" == content = usage user content
    | "~kick " `isPrefixOf` content = write "KICK" (drop 6 content)
    | "~kick" == content = usage user content
    | "~me " `isPrefixOf` content = action user (drop 4 content)
    | "~me" == content = usage user content
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` content = write "PRIVMSG" (drop 5 content)
    | "~op " `isPrefixOf` content = setop user (drop 4 content)
    | "~op" == content = usage user content
    | "~opme" == content = write "MODE" (chan++" +o "++user)
    | "~part " `isPrefixOf` content = write "PART" (drop 6 content)
    | "~part" == content = usage user content
    | "~topic " `isPrefixOf` content = write ("TOPIC "++chan) (" :"++drop 7 content)
    | "~topic" == content = usage user content
    -- | "~verify" == content = privmsg user "Usage is 'verify <password>'"
    -- | "~verify " `isPrefixOf` content = verifyNick user usrreal content
    | otherwise = return ()
 
-- Evaluate commands sent as private messages
evalprivcmd :: String -> String -> Net ()
evalprivcmd user content
    | "!cli" == content = privmsg user clilink
    | "!commands" == content = listcom user
    | "!source" == content = privmsg user source
    | otherwise = return ()
 
-- Evaluate in channel commands
evalchancmd :: String -> String -> String -> Net ()
evalchancmd user origin content
    | "!cli" == content = privmsg origin clilink
    | "!commands" == content = listcom origin
    | "!source" == content = privmsg origin source
    -- Evaluate channel specific commands
    | otherwise = evalAddons user origin content

---- List bot commands here

-- Perform an action
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
action :: String -> String -> Net ()
action user content = let {
    breakwords = words
    ; dest = (!! 0) . words
    ; func = tail -- need a way to drop the first word from the tail
    } in if length (breakwords content) < 2
          then usage user "~action"
          else privmsg (dest content) ("\001ACTION "++(func content)++"\001")

-- Auto identify on login (uses password stored in local file '../.password')
identify :: Net ()
identify = do
    -- Password file should be stored in same location as Readme and .gitignore
    -- (one folder above src/Main.hs)
    password <- io (readFile "../.password")
    privmsg "nickserv" ("identify "++password)

-- Perform any neccessary actions before logging off/quitting
processquit :: Net ()
processquit = do
    write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile ".password")
    write "NICK" "HaskellBot"
    privmsg "nickserv" ("regain "++nick++" "++password)
    privmsg "nickserv" ("regain "++nick++" "++password)
    write "JOIN" chan

-- Revoke op privs from a user
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
revop :: String -> String -> Net ()
revop user content = let {
    breakwords = words
    ; dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in if length (breakwords content) < 2
          then usage user "~deop"
          else write ("MODE "++(dest content)++" -o") (mode content)

-- Assign op privs to a user in any channel we have op privs in
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
setop :: String -> String -> Net ()
setop user content = let {
    breakwords = words
    ; dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in if length (breakwords content) < 2
          then usage user "~op"
          else write ("MODE "++(dest content)++" +o") (mode content)
