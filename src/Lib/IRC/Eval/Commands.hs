module Lib.IRC.Eval.Commands (
    -- Command listing
    listadcom
    , listcom
    -- Command evaluation
    , evaladcmd
    , evalchancmd
    , evalgodcmd
    , evalprivcmd
    -- Export from HabCommands
    , identify
    , joinchan
    , mayberejoin
    , regainnick
    ) where

--import Data.Either.Utils
--import Control.Monad.Error
import Data.List
import qualified Data.Text as T
--import System.IO.Unsafe

-- Hackage modules
--import Data.ConfigFile

-- Local modules
import Addons.IRC.Common
import Lib.IRC.Eval.HabCommands
import Lib.IRC.Net.Socket
import Lib.IRC.Net.Write

---- Resources

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
    listAddonsAdmin user

---- Command evaluation

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
