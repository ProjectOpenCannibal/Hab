module Lib.Eval.Commands (
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
import Control.Monad.Error
import Data.List
import qualified Data.Text as T
import System.IO.Unsafe
import System.Exit

-- Hackage modules
import Data.ConfigFile

-- Local modules
import Addons.IRC.Common
import Lib.Eval.Users
import Lib.Net.Socket
import Lib.Write

---- Resources

-- These will all be moved into a resource file
--
-- common links (available in all channels)
clilink = "http://terokarvinen.com/command_line.html"

---- Command listing

-- List non-admin commands
listcom :: String -> Net ()
listcom user = do
    write ("PRIVMSG "++user++" :") ("Currently supported commands are as follows:")
    write ("PRIVMSG "++user++" :") ("!commands, !cli and !source")
    -- List any commands from our addons
    listAddons user

-- List admin commands
listadcom :: String -> Net ()
listadcom user = do
    write ("PRIVMSG "++user++" :") ("Currently supported admin commands are as follows:")
    write ("PRIVMSG "++user++" :") ("~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part")
    write ("PRIVMSG "++user++" :") ("Please note ~me may be relocated")

---- Command evaluation

-- Process god commands
evalgodcmd :: String -> String -> String -> Net ()
evalgodcmd user usrreal content
    | "~quit" == content = processquit
    | otherwise = return ()

-- Process admin evaluation in the same way as gods
--
-- sndnick -> sndreal -> content (command)
evaladcmd :: String -> String -> String -> Net ()
evaladcmd user usrreal content
    | "~commands" == content = listadcom user
    | "~deftopic" == content = write ("TOPIC"++chan) (" :"++deftopic)
    -- | "~deop " `isPrefixOf` content = revop (drop 6 content)
    -- | "~deop" == content = write "PRIVMSG" (user++" :Usage '~deop <nick> <channel>'")
    | "~id " `isPrefixOf` content = privmsg (drop 4 content)
    | "~id" == content = do
        write "PRIVMSG" (user++" :Usage: '~id <msg>'")
        write "PRIVMSG" (user++" :directs message to primary channel.")
    | "~join " `isPrefixOf` content = write "JOIN" (drop 6 content)
    | "~join" == content = write "PRIVMSG" (user++" :Usage: '~join <channel>'")
    | "~kick " `isPrefixOf` content = write "KICK" (drop 6 content)
    | "~kick" == content = write "PRIVMSG" (user++" :Usage: '~kick <channel> <nick> :<message>'")
    -- | "~me " `isPrefixOf` content = action (drop 4 content)
    -- | "~me" == content = write "PRIVMSG" (user++" :Usage '~me <channel> <action>'")
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example :<message>
    | "~msg " `isPrefixOf` content = write "PRIVMSG" (drop 5 content)
    -- | "~op " `isPrefixOf` content = setop (drop 4 content)
    -- | "~op" == content = write "PRIVMSG" (u++" :Usage '~op <nick> <channel>'")
    | "~opme" == content = write "MODE" (chan++" +o "++user)
    | "~part " `isPrefixOf` content = write "PART" (drop 6 content)
    | "~part" == content = write "PRIVMSG" (user++" :Usage: '~part <channel>'")
    | "~topic " `isPrefixOf` content = write ("TOPIC "++chan) (" :"++drop 7 content)
    | "~topic" == content = do
        write "PRIVMSG" (user++" :Usage: '~topic <topic>'")
        write "PRIVMSG" (user++" :please note this applies to "++chan++" only.")
    -- | "~verify" == content = write "PRIVMSG" (user++" : Usage is 'verify <password>'")
    -- | "~verify " `isPrefixOf` content = verifyNick user usrreal content
    | otherwise = return ()
 
-- Evaluate commands sent as private messages
evalprivcmd :: String -> String -> Net ()
evalprivcmd user content
    | "!cli" == content = write "PRIVMSG" (user++" :"++clilink)
    | "!commands" == content = listcom user
    | "!source" == content = write "PRIVMSG" (user++" :"++source)
    | otherwise = return ()
 
-- Evaluate in channel commands
evalchancmd :: String -> String -> String -> Net ()
evalchancmd user origin content
    | "!cli" == content = write "PRIVMSG" (origin++" :"++clilink)
    | "!commands" == content = listcom origin
    | "!source" == content = write "PRIVMSG" (origin++" :"++source)
    -- Evaluate channel specific commands
    | otherwise = evalAddons user origin content

-- List bot commands here

-- get our commands
--getcommand :: String -> String
--getcommand content = do
    --cp <- unsafePerformIO $ readfile emptyCP "res/commands.cfg"
    --r <- get cp "DEFAULT" content
    --return r

-- Auto identify on login (uses password stored in local file '../.password')
identify :: Net ()
identify = do
    -- Password file should be stored in same location as Readme and .gitignore
    -- (one folder above src)
    password <- io (readFile "../.password")
    write "PRIVMSG" ("nickserv :identify "++password)

-- Perform any neccessary actions before logging off/quitting
processquit :: Net ()
processquit = do
    write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile ".password")
    write "NICK" "HaskellBot"
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "JOIN" chan

-- These are all broken; provides this error :
-- Exception: Prelude.(!!): index too large
--
-- Perform an action
action :: String -> Net ()
action content = let {
    dest = (!! 0) . words
    ; func = (!! 1) . words
    } in write ("PRIVMSG"++(dest content)++" :") ("\001ACTION "++(func content)++"\001")

-- Assign op privs to a user in any channel we have op privs in
setop :: String -> Net ()
setop content = let {
    dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in write ("MODE "++(dest content)++" +o") (mode content)

-- Revoke op privs from a user
revop :: String -> Net ()
revop content = let {
    dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in write ("MODE "++(dest content)++" -o") (mode content)
