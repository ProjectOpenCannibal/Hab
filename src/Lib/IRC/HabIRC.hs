module Lib.IRC.HabIRC (
    ---- export net listen
    listen
    ---- export net socket
    , chan
    , deftopic
    , nick
    , realname
    , server
    , source
    -- Net / Bot monad
    , Bot(socket)
    , Net
    -- Functions
    , connect
    , io
    ---- export net write
    , privmsg
    , write
    ) where

import Lib.IRC.Net.HabIRCNet
