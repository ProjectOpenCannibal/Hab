import Data.List
import Network
import System.IO
import System.Exit
--import Control.Arrow
--import Control.Monad.Reader
--import Control.Exception
import Text.Printf
--import Prelude hiding (catch)

server	= "irc.freenode.org"
port	= 6667
chan	= "#projectopencannibal"
nick	= "CannibalBot"

-- Thread our socket actions through a Net monad
--data Bot = Bot { socket :: Handle }
--type Net = ReaderT Bot IO

--main :: IO ()
--main = bracket connect disconnect loop
	--where
	  --disconnect	= hClose . socket
	  --loop st		= catch (runReaderT run st) (const $ return ())

main = do
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	write h "NICK" nick
	write h "USER" (nick++" 0 * :POC Bot")
	write h "JOIN" chan
	listen h

-- Connect to the server and initialize the bot
--connect :: IO Bot
--connect = notify $ do
	--h <- connectTo server (PortNumber (fromIntegral port))
	--hSetBuffering h NoBuffering
	--return (Bot h)
  --where
	--notify a = bracket
		--(printf "Connecting to %s ... " server >> hFlush stdout)
		--(putStrLn "done.")
		--a

-- Join a channel and initialize our listener
--run :: Net ()
--run = do
	--write "NICK" nick
	--write "USER" (nick++" 0 * :POC Bot")
	--write "JOIN" chan
	--asks socket >>= listen

-- Listen to the socket and respond
listen :: Handle -> IO ()
listen h = forever $ do
	--s <- init `fmap` io (hGetLine h)
	--io (putStrLn s)
	t <- hGetLine h
	let s = init t
	if ping s then pong s else eval h (clean s)
  where
	forever a = a >> forever a
	clean	= drop 1 . dropWhile (/= ':') . drop 1
	ping x	= "PING :" `isPrefixOf` x
	pong x	= write h "PONG" (':' : drop 6 x)

-- Evaluate a command
eval :: Handle -> String -> IO ()
eval h    "!quit"                = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _   _                       = return () -- ignore everything else

-- Add an IO reference to pass data to our net monad (utilized in write)
--io :: IO a -> Net a
--io = liftIO

-- Send a message to the server (only if it's initialized)
write :: Handle -> String -> String -> IO ()
write h s t = do
	--h <- asks socket
	hPrintf h "%s %s\r\n" s t
	printf    "> %s %s\n" s t

-- Wrap write up as a private message
privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
