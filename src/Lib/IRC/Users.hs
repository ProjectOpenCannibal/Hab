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
   This is closer to working, as it is it compiles and loads but even a valid
   password comes up as invalid currently.
-}
verifyNick :: String -> String -> String -> Net ()
verifyNick user usrreal pass = do
    if isGod user || isAdmin user
        then do
            -- Use habs password for testing but change this to allow for
            -- different admins to use different passwords.
            password <- io (readFile "../.password")
            if pass == password
                then do
                     b <- get
                     map <- gets confirmedAdmins
                     put $ b { confirmedAdmins = Map.insert user usrreal map }
                     privmsg user "Nick verified, thank you."
                else privmsg user ("Invalid password <"++pass++">.")
        else privmsg user "Nick not recognized."
