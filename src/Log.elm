module Log exposing (error)

import Ports


error : String -> Cmd msg
error str =
    Ports.logError str
