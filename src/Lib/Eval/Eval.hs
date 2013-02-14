module Lib.Eval.Eval (
    eval
    , evalmode
    ) where

import Data.List
import qualified Data.Text as T

-- Local modules
import Lib.Eval.Commands
import Lib.Eval.Users
import Lib.Net.Socket
import Lib.Write

-- Evaluate a command
--
-- sndnick -> sndreal ->origin -> msgtype -> content (command)
eval :: String -> String -> String -> String -> String -> Net ()
eval user usrreal origin msgtype content
    -- Error codes such as someone else using the bot's NICK come in as type
    -- and should be checked before anything else
    | "433" == msgtype = regainnick
    | "437" == msgtype = regainnick
    -- Check 'NOTICE' messages prior to 'PRIVMSG'
    | "NOTICE" == msgtype
        -- Use this for identify instead of calling it automatically
        -- (current identify tries to identify itself before NickServ notices
        -- in the of a NICK regain)
        = let isIdRequest x = "This nickname is registered. " `isPrefixOf` x
            in if isPriv origin && isIdRequest content
                then identify
                else return ()
    -- Confirm the type instead of switching on all messages.
    | "PRIVMSG" ==  msgtype
        = if isPriv origin
            then do
                if isGod user
                --if isGod user && isAdminConfirmed user usrreal
                    then do
                        evalgodcmd user usrreal content
                        evaladcmd user usrreal content
                        evalprivcmd user content
                    else if isAdmin user
                    --if isAdmin user && isAdminConfirmed user usrreal
                        then do
                            evaladcmd user usrreal content
                            evalprivcmd user content
                    --else if isGod user || isAdmin user
                        --then do
                            --privmsg user "Your nick is recognized as an admin but you are not verified..."
                            --privmsg user "Please verify your nick to use admin commands."
                            --evalprivcmd user content
                    else evalprivcmd user content
            else evalchancmd user origin content
    | otherwise = return ()

-- Evaluate a MODE change
--
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode origin "-o" nick = privmsg "chanserv" ("op "++origin++" "++nick)
evalmode _ _ _ = return ()

-- Check if a given message came in as a PRIVMSG instead of via a channel
isPriv :: String -> Bool
isPriv user = nick == user
