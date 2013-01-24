module Write (write, privmsg) where

import Control.Monad.Reader
import Text.Printf

--Local modules
import Socket

-- Send a message to the server (only if it's initialized)
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Wrap write up as a private message
privmsg :: String -> Net ()  	
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
