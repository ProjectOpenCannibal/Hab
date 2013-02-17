module Lib.IRC.Net.HabIRCNet (
    ---- export listen
    listen
    ---- export socket
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
    , joinchan
    , privmsg
    , write
    ) where

import Lib.IRC.Net.Listen
import Lib.IRC.Net.Socket
