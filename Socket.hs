module Socket (server, port, chan, nick, realname, deftopic, source, Bot(socket), Net, connect, io) where

import Control.Exception
import Control.Monad.Reader
import Network
import System.IO
import Text.Printf

-- Define our channel variables in Socket for easy importation
server = "irc.freenode.org"
port = 6667
chan = "#KF2-dev"
nick = "FMKilo-bot"
realname = "FMKilo"
deftopic = "TWEEZERMOD IS GOD!!! CABLELESS FTW!!! com.powerpoint45.FMKilo!!! MooRom!!! COMMUNAL BATHS FTW!!! KLP ROM FOR KF2? HASHFIRE!!! NO BRONIES ALLOWED!!! ON ENTRY, SAY, 'hello FMKilo-bot'"
source = "https://github.com/FMKilo/Hab/commits/master"
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
