module Lib.IRC.Eval (
    eval          -- String -> String -> String -> String -> String -> Net ()
    , evalmode    -- String -> String -> String -> Net ()
    , listen      -- Handle -> Net ()
    -- Exported from HabCommands through Commands
    , mayberejoin -- String -> Net ()
    ) where

import Data.List
import qualified Data.Text as T
import Network
import System.IO
import System.Exit

-- Local modules
import Lib.IRC.Commands
import Lib.IRC.Users
import Lib.IRC.Socket

-- Evaluate a command
eval :: String -> String -> String -> String -> String -> Net ()
eval user usrreal origin msgtype content
    -- Error codes such as someone else using the bot's NICK come in as type
    -- and should be checked before anything else
    | "433" == msgtype = regainnick
    | "437" == msgtype = regainnick
    -- Check 'NOTICE' messages prior to 'PRIVMSG'
    | "NOTICE" == msgtype
        -- Use this for identify instead of calling it automatically
        -- (old identify tried to identify itself before NickServ notices
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
                    then evalgodcmd user usrreal content
                    else if isAdmin user
                    --if isAdmin user && isAdminConfirmed user usrreal
                        then evaladcmd user usrreal content
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

-- Listen to the socket and respond
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s
        then pong s
        -- In the event of a 'MODE' change evaluate it in Eval.Eval
        else if modechange s
            then evalmode (origin s) (modetype s) (modwho s)
        -- In the evenf of a 'KICK' message on the socket, check if it's the bot
        else if kick s
            then mayberejoin s
        -- Evaluate all other messages through Eval.Eval as if commands
        else eval (user s) (usrreal s) (origin s) (msgtype s) (content s)
  where
    -- Always listening
    forever a = a >> forever a
    -- Server messages
    kick x = "KICK" == (msgtype x)
    modechange x = "MODE" == (msgtype x)
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    -- Parse down our strings
    content = drop 1 . dropWhile (/= ':') . drop 1
    msgtype = (!! 1) . words
    modwho = (!! 4) . words
    modetype = (!! 3) . words
    origin = (!! 2) . words
    user = drop 1 . takeWhile (/= '!')
    usrreal = drop 1 . (!! 1) . words
