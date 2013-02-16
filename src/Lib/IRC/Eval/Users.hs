module Lib.IRC.Eval.Users (
    -- Verification commands
    isAdmin
    , isGod
    ) where

import Data.List
--import qualified Data.Map as M
import qualified Data.Text as T
--import System.IO

--Local modules
--import Lib.IRC.Net.Socket
--import Lib.IRC.Net.Write

-- Define admins and gods (gods have quit and op assignment controls)
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
--isAdminConfirmed :: String -> String -> Net ()
--isAdminConfirmed user usrreal

-- Verify either admin or god
--verifyNick :: String -> String -> String -> Net ()
--verifyNick user usrreal pass = do
    --if isGod user
        --then do
            -- Use habs password for testing but change this to allow for
            -- different admins to use different passwords.
            --password <- io (readFile ".password")
            --if check pass
                --then do
                     --M.insert user (verified usrreal)
                     --write "PRIVMSG" (user++" :Nick verified, thank you.")
                --else write "PRIVMSG" (user++" :Invalid password.")
        --else write "PRIVMSG" (user++" :Nick not recognized.")
  --where
    --check = pass == password
