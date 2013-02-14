module Lib.Net.Socket (
    -- Resource / connection variables
    chan
    , deftopic
    , nick
    , realname
    , server
    , source
    -- Net / Bot monad
    , Bot(socket)
    , Net
    -- Functions
    , connect
    , io
    ) where

import Control.Exception
import Control.Monad.State
import Network
import System.IO
import Text.Printf

-- Define our channel variables in Socket for easy importation (I want to
-- replace this with a .config file, the password will also be passed from this)
chan = "#projectopencannibal"
deftopic = "Project Open Cannibal and Cannibal Open Touch Recovery | http://www.projectopencannibal.net/ || Say hello to Hab (Haskell Bot) | https://github.com/ProjectOpenCannibal/Hab"
nick = "Hab"
port = 6667
realname = "Hab (Haskell Bot), a simple FOSS IRC bot (obviously written in Haskell) | https://github.com/ProjectOpenCannibal/Hab"
server = "irc.freenode.org"
source = "https://github.com/ProjectOpenCannibal/Hab/commits/"

-- Thread our socket actions through a Net monad
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle }

-- Connect to the server and initialize the bot
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

-- Add an IO reference to pass data to our net monad (utilized in write)
io :: IO a -> Net a
io = liftIO
