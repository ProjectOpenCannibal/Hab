module Write (write, privmsg, listcom, listadcom) where

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

-- List non-admin commands
listcom :: String -> Net ()
listcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported commands are as follows:")
    write ("PRIVMSG "++s++" :") ("!adb, !commands, !cli, !fastboot, !source and !udev")

-- List admin commands
listadcom :: String -> Net ()
listadcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported admin commands are as follows:")
    write ("PRIVMSG "++s++" :") ("~commands, ~join, ~kick, ~me, ~msg and ~part")
    write ("PRIVMSG "++s++" :") ("Please note ~me may be relocated")
