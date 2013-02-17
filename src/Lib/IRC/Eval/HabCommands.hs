module Lib.IRC.Eval.HabCommands (
    action
    , identify
    , processquit
    , mayberejoin
    , regainnick
    , revop
    , setop
    , usage
    , usrmsg
    ) where

import Data.List
import qualified Data.Text as T
--import System.IO.Unsafe
import System.Exit

-- Local modules
import Addons.IRC.Common
import Lib.IRC.Net.Socket

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

-- Perform any neccessary actions before logging off/quitting
processquit :: Net ()
processquit = do
    write "QUIT" ":Reloading, hopefully..." >> io (exitWith ExitSuccess)

-- Regain access if the nick is locked
regainnick :: Net ()
regainnick = do
    password <- io (readFile ".password")
    write "NICK" "HaskellBot"
    privmsg "nickserv" ("regain "++nick++" "++password)
    privmsg "nickserv" ("regain "++nick++" "++password)
    joinchan chan

-- Revoke op privs from a user
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

-- Assign op privs to a user in any channel we have op privs in
-- (code feels redundant, I don't like breakwords in let...)
-- ToDo: write a function to grab dest and func regardless of order passed
setop :: String -> String -> Net ()
setop user content = let {
    breakwords = words
    ; dest = (!! 1) . words
    ; mode = (!! 0) . words
    } in if length (breakwords content) < 2
          then usage user "~op"
          else write ("MODE "++(dest content)++" +o") (mode content)

-- List all of our usage commands for easy reference
usage :: String -> String -> Net ()
usage user content =
    case content of
        "~deop"   -> privmsg user "Usage: '~deop <nick> <channel>'"
        "~id"     -> do
                         privmsg user "Usage: '~id <msg>'"
                         privmsg user ("directs message to "++chan++"only.")
        "~join"   -> privmsg user "Usage: '~join <channel>'"
        "~kick"   -> privmsg user "Usage: '~kick <channel> <nick> :<message>'"
        "~me"     -> privmsg user "Usage: '~me <channel> <action>'"
        "~msg"    -> do
                         privmsg user "Usage: '~msg <dest> <message>'"
                         privmsg user "please note dest may be a user or channel."
        "~op"     -> privmsg user "Usage: '~op <nick> <channel>'"
        "~part"   -> privmsg user "Usage: '~part <channel>'"
        "~topic"  -> do
                         privmsg user "Usage: '~topic <topic>'"
                         privmsg user ("please note this applies to "++chan++" only.")
        otherwise -> usageAddons user content

-- Send a message (to channel or nick)
-- (code feels redundant, I don't like breakwords in let...)
usrmsg :: String -> String -> Net ()
usrmsg user content = let {
    breakwords = words
    ; dest = (!! 0) . words
    ; msg = tail -- need a way to drop the first word from the tail
    } in if length (breakwords content) < 2
        then usage user "~msg"
        else privmsg (dest content) (msg content)
