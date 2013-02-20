module Lib.IRC.Socket (
    -- Resource / connection variables
    chan                   -- String
    , nick                 -- String
    , realname             -- String
    , server               -- String
    , source               -- String
    -- Net monad
    , Net                  -- StateT Bot IO
    -- Data storage / types
    , Bot(socket, seenMap) -- Bot (Handle, SeenEntry)
    , SeenEntry(SeenEntry) -- SeenEntry String String Clocktime
    -- Functions
    , connect              -- IO Bot
    , io                   -- IO a -> Net a
    , joinchan             -- String -> Net ()
    , privmsg              -- String -> String -> Net ()
    , write                -- String -> String -> Net ()
    ) where

import qualified Control.Exception as E
import Control.Monad.State
import qualified Data.Map as M
import Network
import System.IO
import System.Time
import Text.Printf

-- Define our channel variables in Socket for easy importation (I want to
-- replace this with a .config file, the password will also be passed from this)
chan = "#projectopencannibal"
nick = "Hab"
port = 6667
realname = "Hab (Haskell Bot), a simple FOSS IRC bot (obviously written in Haskell) | https://github.com/ProjectOpenCannibal/Hab"
server = "irc.freenode.org"
source = "https://github.com/ProjectOpenCannibal/Hab/commits/"

-- Thread our socket actions through a Net monad
type Net = StateT Bot IO

data Bot = Bot {
    socket  :: Handle,
    seenMap :: M.Map String SeenEntry
    }
data SeenEntry = SeenEntry String String ClockTime

-- Connect to the server and initialize the bot
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h M.empty)
  where
    notify = E.bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

-- Add an IO reference to pass data to our net monad (utilized in write)
io :: IO a -> Net a
io = liftIO

-- Join a channel
joinchan :: String -> Net ()
joinchan channel = write "JOIN" channel

-- Wrap write up as a private message
privmsg :: String -> String -> Net ()
privmsg dest content = write "PRIVMSG" (dest++" :"++content)

-- Send a message to the server (only if it's initialized)
write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
