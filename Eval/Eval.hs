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
    isPriv x = nick == x

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
