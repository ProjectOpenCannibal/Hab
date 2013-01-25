module Socket (server, port, chan, nick, realname, deftopic, source, Bot(socket), Net, connect, io) where

import Control.Exception
import Control.Monad.Reader
import Network
import System.IO
import Text.Printf

-- Define our channel variables in Socket for easy importation (I want to
-- replace this with a .config file, the password will also be passed from this)
server = "irc.freenode.org"
port = 6667
chan = "#projectopencannibal"
nick = "Hab"
realname = "Hab (Haskell Bot), a simple FOSS IRC bot (obviously written in Haskell) | https://github.com/ProjectOpenCannibal/CannibalismBot"
deftopic = "Project Open Cannibal and Cannibal Open Touch Recovery | http://www.projectopencannibal.net/ || Say hello to Hab (Haskell Bot) | https://github.com/ProjectOpenCannibal/CannibalismBot"
source = "https://github.com/ProjectOpenCannibal/CannibalismBot/commits/"

-- Thread our socket actions through a Net monad
data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

-- Connect to the server and initialize the bot
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- Add an IO reference to pass data to our net monad (utilized in write)
io :: IO a -> Net a
io = liftIO
