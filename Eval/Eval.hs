module Eval.Eval (eval, evalmode, mayberejoin) where

import Data.List
import qualified Data.Text as T

-- Local modules
import Eval.Commands
import Eval.Users
import Socket
import Write

-- Evaluate a command
--
-- SndNick -> Sndreal -> Origin -> Msgtype -> content (command)
-- (we drop type for now cause nothing uses it)
eval :: String -> String -> String -> String -> String -> Net ()
eval u r o _ c = let isPriv x = nick == x
    in if isPriv o
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

-- Evaluate a MODE change
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode c "-o" nick = write "PRIVMSG" ("chanserv :op "++c++" "++nick)
evalmode _ _ _ = return ()

-- Check who was kicked and if it was the bot, rejoin the channel in question
mayberejoin :: String -> Net ()
mayberejoin s = do
    if check s
        then write "JOIN" (origin s)
        else return ()
  where
    check x = nick == (whois s)
    origin = (!! 2) . words
    whois = (!! 3) . words
