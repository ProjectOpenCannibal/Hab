import Data.List
import Network
import System.IO
import System.Exit
import Control.Monad.Reader
--import Control.OldException
import Control.Exception
import Text.Printf
--import Prelude hiding (catch)

server	= "irc.freenode.org"
port	= 6667
chan	= "#projectopencannibal"
nick	= "CannibalismBot"

-- Thread our socket actions through a Net monad
data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
	where
	  disconnect	= hClose . socket
	  --Old.Exception isn't loading in Arch so we're leaving the catch out of
	  --this for now.
	  loop st		= runReaderT run st

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

-- Join a channel and initialize our listener
run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick++" 0 * :POC Bot")
	write "JOIN" chan
	asks socket >>= listen

-- Listen to the socket and respond
listen :: Handle -> Net ()
listen h = forever $ do
	s <- init `fmap` io (hGetLine h)
	io (putStrLn s)
	if ping s then pong s else eval (clean s)
  where
	forever a = a >> forever a
	clean	= drop 1 . dropWhile (/= ':') . drop 1
	ping x	= "PING :" `isPrefixOf` x
	pong x	= write "PONG" (':' : drop 6 x)

-- Evaluate a command
eval :: String -> Net ()
eval 	"!quit"					= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x	= privmsg (drop 4 x)
eval	_						= return () -- ignore everything else

-- Send a message to the server (only if it's initialized)
write :: String -> String -> Net ()
write s t = do
	h <- asks socket
	io $ hPrintf h "%s %s\r\n" s t
	io $ printf    "> %s %s\n" s t

-- Wrap write up as a private message
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Add an IO reference to pass data to our net monad (utilized in write)
io :: IO a -> Net a
io = liftIO
