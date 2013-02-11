module Eval.Eval (
    eval
    , evalmode
    ) where

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
eval :: String -> String -> String -> String -> String -> Net ()
eval u r o t c
    -- Error codes such as someone else using the bot's NICK come in as type
    -- and should be checked before anything else
    | "433" == t = regainnick
    | "437" == t = regainnick
    -- Confirm the type instead of switching on all messages.
    | "PRIVMSG" ==  t = let isPriv x = nick == x
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
    | otherwise = return ()

-- Evaluate a MODE change
-- origin -> modetype (voice, etc) -> modwho (changes whos mode?)
evalmode :: String -> String -> String -> Net ()
evalmode o "-o" nick = write "PRIVMSG" ("chanserv :op "++o++" "++nick)
evalmode _ _ _ = return ()

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile ".password")
    write "NICK" "HaskellBot"
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "PRIVMSG nickserv :regain " (nick++" "++password)
    write "JOIN" chan
