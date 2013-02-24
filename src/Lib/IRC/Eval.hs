module Lib.IRC.Eval (
    eval          -- String -> String -> String -> String -> String -> Net ()
    , evalmode    -- String -> String -> String -> Net ()
    , listen      -- Handle -> Net ()
    ) where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Network
import System.IO
import System.Exit
import Text.Printf

-- Local modules
import Lib.IRC.Commands
import Lib.IRC.Users
import Lib.IRC.Socket

-- Evaluate a command
eval :: String -> String -> String -> String -> String -> Net ()
eval user usrreal origin msgtype content
    -- Error and server codes (such as someone else using the bot's NICK) come
    -- through in the form of msgtype and should be checked first.
    {-
    Need a way to store the lastuser to send a command to the bot temporarily
    so that we can return error messages if neccessary.

    Couldn't match expected type `StateT Bot IO ()'
                with actual type `String -> Net ()'
    In a stmt of a 'do' block:
      privmsg
      $ (maybe ("IngCr3at1on") (foo) $ Map.lookup last map) content
    In the expression:
      do { map <- gets lastUser;
           privmsg
           $ (maybe ("IngCr3at1on") (foo) $ Map.lookup last map) content }
    In the expression:
      let foo (LastUser u) = printf "%s" u
      in
        do { map <- gets lastUser;
             privmsg
             $ (maybe ("IngCr3at1on") (foo) $ Map.lookup last map) content }

    -}{-
    | "401" == msgtype -- Failed msg, no such nick/channel.
        = let foo (LastUser u) = printf "%s" u
            in do
                map <- gets lastUser
                privmsg $ (maybe ("IngCr3at1on") (foo) $ Map.lookup last map) content
    | "403" == msgtype -- Failed join, no such channel.
        = let foo (LastUser u) = printf "%s" u
            in do
                map <- gets lastUser
                privmsg $ (maybe ("IngCr3at1on") (foo) $ Map.lookup last map) content
    -}
    | "433" == msgtype = regainnick
    | "437" == msgtype = regainnick
    -- Check 'NOTICE' messages prior to 'PRIVMSG'
    | "NOTICE" == msgtype
        = let {
            -- Used for identification.
            isIdRequest x = "This nickname is registered. " `isPrefixOf` x
        }
        -- Identify but only to NickServ.
        in if isPriv origin && isIdRequest content && user == "NickServ"
            then identify
            else return ()
    -- Confirm the type instead of switching on all messages.
    | "PRIVMSG" ==  msgtype
        = if isPriv origin
            then do
                -- Set the last user to send a private message command to Hab.
                {-
                b <- get
                map <- gets lastUser
                put $ b { lastUser = Map.insert last (LastUser user) map }
                -}
                {-
                if isGod user || isAdmin user && isAdminConfirmed user usrreal
                    then do
                -}
                        if isGod user
                            then evalgodcmd user usrreal content
                            else if isAdmin user
                                then evaladcmd user usrreal content
                            else evalprivcmd user content
                    --else evalprivcmd user content
            else do
                updateSeenMap user origin content
                evalchancmd user origin content
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
        -- In the event of a 'MODE' change, see if it's the bot and react
        else if modechange s
            then evalmode (origin s) (modetype s) (modwho s)
        -- In the event of a 'KICK' message on the socket, check if it's the bot
        else if kick s
            then mayberejoin s
        -- Evaluate all other messages
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
    content x = drop 1 (dropWords 3 x)
    msgtype = (!! 1) . words
    modwho = (!! 4) . words
    modetype = (!! 3) . words
    origin = (!! 2) . words
    user = drop 1 . takeWhile (/= '!')
    usrreal = drop 1 . (!! 1) . words
