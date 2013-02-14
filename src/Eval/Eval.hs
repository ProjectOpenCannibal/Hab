module Eval.Eval (
    eval
    , evalmode
    ) where

import Data.List
import qualified Data.Text as T

-- Local modules
import Eval.Commands
import Eval.Users
import Net.Socket
import Write

-- Evaluate a command
--
-- sndnick -> sndreal ->origin -> msgtype -> content (command)
eval :: String -> String -> String -> String -> String -> Net ()
eval u r o t c
    -- Error codes such as someone else using the bot's NICK come in as type
    -- and should be checked before anything else
    | "433" == t = regainnick
    | "437" == t = regainnick
    -- Check 'NOTICE' messages prior to 'PRIVMSG'
    | "NOTICE" == t
        -- Use this for identify instead of calling it automatically
        -- (current identify tries to identify itself before NickServ notices
        -- in the of a NICK regain)
        = let isIdRequest x = "This nickname is registered. " `isPrefixOf` x
            in if isPriv o && isIdRequest c
                then identify
                else return ()
    -- Confirm the type instead of switching on all messages.
    | "PRIVMSG" ==  t
        = if isPriv o
            then do
                if isGod u
                --if isGod u && isAdminConfirmed u r
                    then do
                        evalgodcmd u r c
                        evaladcmd u r c
                        evalprivcmd u c
                    else if isAdmin u
                    --if isAdmin u && isAdminConfirmed u r
                        then do
                            evaladcmd u r c
                            evalprivcmd u c
                    --else if isGod u || isAdmin u
                        --then do
                            --write "PRIVMSG" (u++" :"++"Your nick is recognized as an admin but you are not verified...")
                            --write "PRIVMSG" (u++" :"++"Please verify your nick to use admin commands.")
                            --evalprivcmd u c
                    else evalprivcmd u c
            else evalchancmd u o c
    | otherwise = return ()

-- Evaluate a MODE change
--
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode o "-o" nick = write "PRIVMSG" ("chanserv :op "++o++" "++nick)
evalmode _ _ _ = return ()

-- Check if a given message came in as a PRIVMSG instead of via a channel
--
-- sndnick
isPriv :: String -> Bool
isPriv u = nick == u
