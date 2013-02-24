module Lib.IRC.HabCommands (
    action          -- String -> String -> Net ()
    , dropWord
    , dropWords
    , identify      -- Net ()
    , mayberejoin   -- String -> Net ()
    , processquit   -- Net ()
    , regainnick    -- Net ()
    , revop         -- String -> String -> Net ()
    , seen          -- String -> String -> Net ()
    , setop         -- String -> String -> Net ()
    , usage         -- String -> String -> Net ()
    --, usrmsg        -- String -> String -> Net ()
    , updateSeenMap -- String -> String -> String -> Net ()
    ) where

import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
--import System.IO.Unsafe
import System.Exit
import System.Time
import Text.Printf

-- Local modules
import Addons.IRC.IRCCommon
import Lib.IRC.Socket

---- Commands called directly by Hab

-- Perform an action
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
action :: String -> String -> Net ()
action user content = let {
    breakwords = words
    ; dest = (!! 0) . words
    ; func = tail -- need a way to drop the first word from the tail
    } in if length (breakwords content) < 2
          then usage user "~action"
          else privmsg (dest content) ("\001ACTION "++(func content)++"\001")

{- Add a function for dropping a word off the front of a string
   (Thanks ahihi in Haskell IRC on Freenode) -}
--dropWord :: String
dropWord = dropWhile isSpace . dropWhile (not . isSpace) . dropWhile isSpace;

{- Drop a number of words from the front of a string
   (Thanks ahihi in Haskell IRC on Freenode) -}
--dropWords :: String -> String -> String
dropWords n s = iterate dropWord s !! n

-- Auto identify on login (uses password stored in local file '../.password')
identify :: Net ()
identify = do
    -- Password file should be stored in same location as Readme and .gitignore
    -- (one folder above src/Main.hs)
    password <- io (readFile "../.password")
    privmsg "nickserv" ("identify "++password)

-- Check who was kicked and if it was the bot, rejoin the channel in question
mayberejoin :: String -> Net ()
mayberejoin s = do
    if check s
        then joinchan (origin s)
        else return ()
  where
    check x = nick == (whois s)
    origin = (!! 2) . words
    whois = (!! 3) . words

-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

-- Perform any neccessary actions before logging off/quitting.
processquit :: Net ()
processquit = do
    write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Prompt admins to verify their nickname.
prompttoverify :: String -> Net ()
prompttoverify user = do
    privmsg user "Your nick is recognized as an admin but you are not verified..."
    privmsg user "Please verify your nick to use admin commands."

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile "../.password")
    write "NICK" "HaskellBot"
    privmsg "nickserv" ("regain "++nick++" "++password)
    privmsg "nickserv" ("regain "++nick++" "++password)
    joinchan chan

-- Revoke op privs from a user.
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
revop :: String -> String -> Net ()
revop user content = let {
    breakwords = words
    ; dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in if length (breakwords content) < 2
          then usage user "~deop"
          else write ("MODE "++(dest content)++" -o") (mode content)

-- Check our SeenMap and list when we last saw a given user.
seen :: String -> String -> Net ()
seen user origin = do
    map <- gets seenMap
    now <- io getClockTime
    privmsg origin $ maybe (printf "I have not seen '%s'" user) (foo now) $ Map.lookup user map
  where
    foo curTime (SeenEntry o m t) =
        printf "%s was last seen in %s %s ago saying: %s" user o (pretty $ diffClockTimes curTime t) m

-- Assign op privs to a user in any channel we have op privs in.
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed.
setop :: String -> String -> Net ()
setop user content = let {
    breakwords = words
    ; dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in if length (breakwords content) < 2
          then usage user "~op"
          else write ("MODE "++(dest content)++" +o") (mode content)

-- Update our map of people that we've seen.
updateSeenMap :: String -> String -> String -> Net ()
updateSeenMap user origin content = do
    b <- get
    map  <- gets seenMap
    time <- io getClockTime
    put $ b { seenMap = Map.insert user (SeenEntry origin content time) map }

-- List all of our usage commands for easy reference.
usage :: String -> String -> Net ()
usage user content =
    case content of
        -- For the sake of order list admin commands first.
        "~deop"   -> privmsg user "Usage: '~deop <nick> <channel>'."
        "~id"     -> do
                         privmsg user "Usage: '~id <msg>',"
                         privmsg user ("directs message to "++chan++"only.")
        "~join"   -> privmsg user "Usage: '~join <channel>'."
        "~kick"   -> privmsg user "Usage: '~kick <channel> <nick> :<message>'."
        "~me"     -> privmsg user "Usage: '~me <channel> <action>'."
        "~msg"    -> do
                         --privmsg user "Usage: '~msg <dest> <message>'."
                         privmsg user "Usage: '~msg <dest> :<message>',"
                         privmsg user "please note dest may be a user or channel."
        "~op"     -> privmsg user "Usage: '~op <nick> <channel>'."
        "~part"   -> privmsg user "Usage: '~part <channel>'."
        "~topic"  -> do
                         privmsg user "Usage: '~topic <topic>',"
                         privmsg user ("please note this applies to "++chan++" only.")
        "~verify" -> privmsg user "Usage: '~verify <password>'."
        -- List non-admin commands
        "!seen"   -> privmsg user "Usage: '!seen <nick>'."
        otherwise -> usageAddons user content

-- Send a message (to channel or nick).
-- (code feels redundant, I don't like breakwords in let...)
{- Compile Error:
Lib/IRC/HabCommands.hs:121:21:
    Couldn't match expected type `[String]'
                with actual type `String -> [String]'
    In the first argument of `unwords', namely `rawmsg'
    In the expression: unwords rawmsg
    In an equation for `msg': msg = unwords rawmsg
-}
{-
usrmsg :: String -> String -> Net ()
usrmsg user content = let {
    breakwords = words
    ; dest = (!! 0) . words
    ; rawmsg = tail . words
    ; msg = unwords rawmsg
    } in if length (breakwords content) < 2
        then usage user "~msg"
        else privmsg (dest content) (msg content)
-}
