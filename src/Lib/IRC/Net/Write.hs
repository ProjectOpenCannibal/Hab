module Lib.IRC.Net.Write (
    privmsg
    , write
    ) where

import Control.Monad.State
import Text.Printf

--Local modules
import Lib.IRC.Net.Socket

-- Send a message to the server (only if it's initialized)
write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Wrap write up as a private message
privmsg :: String -> String -> Net ()
privmsg dest content = write "PRIVMSG" (dest++" :"++content)
