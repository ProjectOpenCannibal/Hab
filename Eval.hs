module Eval (eval) where

import Data.List
import System.Exit

-- Local modules
import Socket
import Write

-- Evaluate a command
--
-- SndNick -> SndFrom (channel/privchat etc) -> content (command)
eval :: String -> String -> String -> Net ()
-- Non-argumental commands (keep in alpha)
-- I'm unable to make this apply to a list of users instead of the single option
eval "FMKilo" _ "!quit" = write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)
eval y _ "Hello FMKilo-bot" = privmsg ("Hello "++y)
eval y _ "hello FMKilo-bot" = privmsg ("Hello "++y)
eval y _ "Hello fmkilo-bot" = privmsg ("Hello "++y)
eval y _ "hello fmkilo-bot" = privmsg ("Hello "++y)
eval "FMKilo" _ "!deftopic" = write ("TOPIC "++chan) (" :"++deftopic)
eval _ _ "brick" = privmsg "boned"
eval _ _ "What is the answer to life, the universe and everything?" = privmsg "forty-two"
eval _ _ x
--    | "FMKilo" `isPrefixOf` x = write "PRIVMSG #kf2-dev :FMKilo is a fucking badass!!!" (drop 70000 x)
    | "penises" `isPrefixOf` x = write "PRIVMSG #kf2-dev :Alright, I'm done with all of these dick references... You can all leave now." (drop 70000 x)
-- Single arg commands (keep in alpha)
eval "FMKilo" _ x
-- I'm unable to make this apply to a list of users instead of the single option
-- for now the only commands included are ones we would want to limit to admins
-- anyway since I'm the only admin (online), the above works (I will want to
-- change it as soon as I figure out how though).
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    -- a cheap implementation of message, only works if you manually do the
    -- channel or nick as #example : <message>
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)-- Single arg commands (keep in alpha)
    | "!sleep" `isPrefixOf` x = write "PRIVMSG #kf2-dev :zzzzzzzzzzzzzzzzzz" (drop 6 x) 
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")

eval "FMKilo-d2usc" _ x
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!topic " `isPrefixOf` x = write ("TOPIC "++chan) (" :"++drop 7 x)
eval "FMKilo-otter2-cm" _ x
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!part " `isPrefixOf` x = write "PART" (drop 6 x)
eval "IngCr3at1on" _ x
    | "!me " `isPrefixOf` x = privmsg ("\001ACTION "++(drop 4 x)++"\001")
    | "!id " `isPrefixOf` x = privmsg (drop 4 x)
    | "!join " `isPrefixOf` x = write "JOIN" (drop 6 x)
    | "!kick " `isPrefixOf` x = write "KICK" (drop 6 x)
    | "!msg " `isPrefixOf` x = write "PRIVMSG" (drop 5 x)
    | "!topic " `isPrefixOf` x = write ("PRIVMSG chanserv :topic "++chan) (drop 7 x)
eval _ _ "!source" = privmsg source

eval _ _ _ = return () -- ignore everything else
--"I think I see how it is, I'm not a yoyo"
