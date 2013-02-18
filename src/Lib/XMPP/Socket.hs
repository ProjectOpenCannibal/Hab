module Lib.XMPP.Socket (
    Net
    , Bot (socket)
    --, connect -- IO Bot
    , io      -- IO a -> Net a
    ) where

import qualified Control.Exception as E
import Control.Monad.State
import Network.XMPP
import System.IO
import Text.Printf

host = "talk.google.com"
port = 5222

-- Thread our socket actions through a Net monad
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle }

-- Connect to the server and initialize the bot
--connect :: IO Bot
--connect = notify $ do
    --h <- connectTo server (PortNumber (fromIntegral port))
    --hSetBuffering h NoBuffering
    --return (Bot h)
  --where
    --notify = E.bracket_
        --(printf "Connecting to %s ... " server >> hFlush stdout)
        --(putStrLn "done.")

-- Add an IO reference to pass data to our net monad (utilized in write)
io :: IO a -> Net a
io = liftIO

-- Join a room
--joinroom :: String -> Net ()
--joinroom channel =

-- Wrap write up as a private message
--privmsg :: String -> String -> Net ()
--privmsg dest content =

-- Send a message to the server (only if it's initialized)
--write :: String -> String -> Net ()
--write s t = do
    --h <- gets socket
    --io $ hPrintf h "%s %s\r\n" s t
    --io $ printf    "> %s %s\n" s t
