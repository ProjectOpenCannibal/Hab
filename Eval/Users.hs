module Eval.Users (gods, admins) where

import Data.List
--import qualified Data.Map as M
import qualified Data.Text as T
--import System.IO

--Local modules
--import Socket
--import Write

-- Define admins and gods (gods have quit and op assignment controls)
gods = ["IngCr3at1on"]
admins = ["IngCr3at1on", "FMKilo", "Hashcode", "iytrix"]

-- Track a map of admin nicknames vs realnames
-- Use this for verifying both admins and gods
-- SndNick -> SndReal
--isAdminConfirmed :: String -> String -> Net ()
--isAdminConfirmed u r

-- Verify either admin or god
-- SndNick -> SndReal -> Password
--verifyNick :: String -> String -> String -> Net ()
--verifyNick u r p = do
    --if u == "IngCr3at1on"
        --then do
            --password <- io (readFile ".password")
            --if check p
                --then do
                     --M.insert u (verified r)
                     --write "PRIVMSG" (u++" :Nick verified, thank you.")
                --else write "PRIVMSG" (u++" :Invalid password.")
        --else write "PRIVMSG" (u++" :Nick not recognized.")
  --where
    --check = p == password
