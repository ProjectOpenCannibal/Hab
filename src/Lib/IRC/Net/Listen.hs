module Lib.IRC.Net.Listen (
    listen
    ) where

import Data.List
import qualified Data.Text as T
import Network
import System.IO
import System.Exit

-- Local modules
import Lib.IRC.Eval.Eval
import Lib.IRC.Net.Socket
import Lib.IRC.Net.Write

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
