module Lib.IRC.Commands (
    -- Command listing
    listadcom       -- String -> Net ()
    , listcom       -- String -> Net ()
    -- Command evaluation
    , evaladcmd     -- String -> String -> String -> Net ()
    , evalchancmd   -- String -> String -> String -> Net ()
    , evalgodcmd    -- String -> String -> String -> Net ()
    , evalprivcmd   -- String -> String -> Net ()
    -- Export from HabCommands
    , identify      -- Net ()
    , mayberejoin   -- String -> Net ()
    , regainnick    -- Net ()
    , updateSeenMap -- String -> String -> String -> Net ()
    ) where

--import Data.Either.Utils
--import Control.Monad.Error
import Data.List
import qualified Data.Text as T
--import System.IO.Unsafe

-- Hackage modules
--import Data.ConfigFile

-- Local modules
import Addons.IRC.IRCCommon
import Lib.IRC.HabCommands
import Lib.IRC.Socket
import Lib.IRC.Users

---- Resources

-- common links (available in all channels)
clilink = "http://terokarvinen.com/command_line.html"

---- Command listing

-- List non-admin commands
listcom :: String -> Net ()
listcom user = do
    privmsg user "Currently supported commands are as follows:"
    privmsg user "!commands, !cli, !seen and !source"
    -- List any commands from our addons
    listAddons user

-- List admin commands
listadcom :: String -> Net ()
listadcom user = do
    privmsg user "Currently supported admin commands are as follows:"
    privmsg user "~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part"
    listAddonsAdmin user

---- Command evaluation

-- Process god commands
evalgodcmd :: String -> String -> String -> Net ()
evalgodcmd user usrreal content
    | "~quit" == content = processquit
    | otherwise = evaladcmd user usrreal content

-- Process admin evaluation in the same way as gods
evaladcmd :: String -> String -> String -> Net ()
evaladcmd user usrreal content
    | "~commands" == content = listadcom user
    | "~deop " `isPrefixOf` content = revop user (drop 6 content)
    | "~deop" == content = usage user content
    | "~id " `isPrefixOf` content = privmsg chan (drop 4 content)
    | "~id" == content = usage user content
    | "~join " `isPrefixOf` content = joinchan (drop 6 content)
    | "~join" == content = usage user content
    | "~kick " `isPrefixOf` content = write "KICK" (drop 6 content)
    | "~kick" == content = usage user content
    | "~me " `isPrefixOf` content = action user (drop 4 content)
    | "~me" == content = usage user content
    -- | "~msg " `isPrefixOf` content = usrmsg user (drop 5 content)
    | "~msg " `isPrefixOf` content = privmsg user (drop 5 content)
    | "~msg" == content = usage user content
    | "~op " `isPrefixOf` content = setop user (drop 4 content)
    | "~op" == content = usage user content
    | "~opme" == content = write "MODE" (chan++" +o "++user)
    | "~part " `isPrefixOf` content = write "PART" (drop 6 content)
    | "~part" == content = usage user content
    | "~topic " `isPrefixOf` content = write ("TOPIC "++chan) (" :"++drop 7 content)
    | "~topic" == content = usage user content
    | "~verify " `isPrefixOf` content = verifyNick user usrreal (drop 8 content)
    | "~verify" == content = usage user content
    -- Before moving on to private command evalation check Addons for any admin
    -- commands (this should always go last)
    | "~" `isPrefixOf` content = evalAddonsAdmin user usrreal content
    | otherwise = evalprivcmd user content
 
-- Evaluate commands sent as private messages
evalprivcmd :: String -> String -> Net ()
evalprivcmd user content
    | "!cli" == content = privmsg user clilink
    | "!commands" == content = listcom user
    {- The second value in seen is origin, if this is received on a privmsg
	   origin is actually parsed out as 'Hab' pass user instead so Hab knows
       who to respond to. -}
    | "!seen " `isPrefixOf` content = seen (drop 6 content) user
    | "!seen" == content = usage user content
    | "!source" == content = privmsg user source
    -- For addons in private message send the user as the origin so's to return
    -- to the correct location.
    | otherwise = evalAddons user user content
 
-- Evaluate in channel commands
evalchancmd :: String -> String -> String -> Net ()
evalchancmd user origin content
    | "!cli" == content = privmsg origin clilink
    | "!commands" == content = listcom origin
    | "!seen " `isPrefixOf` content = seen (drop 6 content) origin
    | "!seen" == content = usage origin content
    | "!source" == content = privmsg origin source
    -- Evaluate channel specific commands
    | otherwise = evalAddons user origin content
