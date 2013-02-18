module Lib.IRC.HabIRC (
    ---- export listen (from eval)
    listen
    ---- export socket
    , chan          -- String
    , deftopic    -- String
    , nick        -- String
    , realname    -- String
    , server      -- String
    , source      -- String
    -- Net / Bot monad
    , Bot(socket) -- Bot (Handle)
    , Net         -- StateT Bot IO
    -- Functions
    , connect     -- IO Bot
    , io          -- IO a -> Net a
    , joinchan    -- String -> Net ()
    , privmsg     -- String -> String -> Net ()
    , write       -- String -> String -> Net ()
    ) where

import Lib.IRC.Eval
import Lib.IRC.Socket
