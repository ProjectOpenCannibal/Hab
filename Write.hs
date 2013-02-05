module Write (write, privmsg, listcom, listadcom) where

import Control.Monad.Reader
import Text.Printf

--Local modules
import Socket

-- Common strings used throughout the file
chanspeccmd = "The following commands are specific to this channel"

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
    write ("PRIVMSG "++s++" :") ("!commands, !cli and !source")
    if kf1talk s
        then do
            write ("PRIVMSG "++s++" :") chanspeccmd
            write ("PRIVMSG "++s++" :") ("!guide and !udev")
        else if kf2talk s
            then do
                write ("PRIVMSG "++s++" :") chanspeccmd
                write ("PRIVMSG "++s++" :") ("!moorom, !oneclick, !rts and !udev")
        else return ()
  where
    kf1talk x = x == "#kindlefire-dev"
    kf2talk x = x == "#kf2-dev"

-- List admin commands
listadcom :: String -> Net ()
listadcom s = do
    write ("PRIVMSG "++s++" :") ("Currently supported admin commands are as follows:")
    write ("PRIVMSG "++s++" :") ("~commands, ~deop, ~join, ~kick, ~me, ~msg, ~op, ~opme and ~part")
    write ("PRIVMSG "++s++" :") ("Please note ~me may be relocated")
