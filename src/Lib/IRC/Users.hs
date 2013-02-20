module Lib.IRC.Users (
    -- Verification commands
    isAdmin      -- String -> Bool
    , isGod      -- String -> Bool
    , verifyNick -- String -> String -> Net ()
    ) where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
--import System.IO

--Local modules
import Lib.IRC.Socket

-- Define admins and gods (god provides quit)
gods = ["IngCr3at1on"]
-- No need to repeat entries as admin commands are evaluated by gods regardless
admins = ["FMKilo",
          "Hashcode",
          "iytrix"]

-- Check if the user is a god
isGod :: String -> Bool
isGod user = user `elem` gods

-- Check if the user is an admin
isAdmin :: String -> Bool
isAdmin user = user `elem` admins

-- Track a map of admin nicknames vs realnames
-- Use this for verifying both admins and gods
{-
isAdminConfirmed :: String -> String -> Net ()
isAdminConfirmed user usrreal
-}

-- Verify either admin or god
{-
   This is closer to working, as it is it compiles and loads properly but when
   you call the command you Hab exits without any listed reason.

:NickServ!NickServ@services. NOTICE Hab :You are now identified for Hab.
:ChanServ!ChanServ@services. MODE #projectopencannibal +o Hab
:IngCr3at1on!~ing@cpe<removed host info> PRIVMSG Hab :~verify
> PRIVMSG IngCr3at1on :Usage: '~verify <password>'.
:IngCr3at1on!~ing@cpe<removed host info> PRIVMSG Hab :~verify test
*Main>
-}
verifyNick :: String -> String -> String -> Net ()
verifyNick user usrreal pass = do
    if isGod user || isAdmin user
        then do
            -- Use habs password for testing but change this to allow for
            -- different admins to use different passwords.
            password <- io (readFile ".password")
            if pass == password
                then do
                     b <- get
                     map <- gets confirmedAdmins
                     put $ b { confirmedAdmins = Map.insert user usrreal map }
                     write "PRIVMSG" (user++" :Nick verified, thank you.")
                else write "PRIVMSG" (user++" :Invalid password.")
        else write "PRIVMSG" (user++" :Nick not recognized.")
