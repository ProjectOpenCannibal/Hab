module Listen (listen) where

import Data.List
import qualified Data.Text as T
import Network
import System.IO
import System.Exit

-- Local modules
import Eval
import Socket
import Write

-- Listen to the socket and respond
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s 
        then pong s
        else if modechange s
            then evalmode (origin s) (modetype s) (modwho s)
        else if kick s
            then mayberejoin s
        else eval (sndnick s) (origin s) (msgtype s) (content s)
  where
    content = drop 1 . dropWhile (/= ':') . drop 1
    forever a = a >> forever a
    kick x = "KICK" == (msgtype x)
    msgtype = (!! 1) . words
    modwho = (!! 4) . words
    modechange x = "MODE" == (msgtype x)
    modetype = (!! 3) . words
    origin = (!! 2) . words
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    sndnick = drop 1 . takeWhile (/= '!')
