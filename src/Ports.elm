port module Ports
    exposing
        ( txLogout
        , txToken
        )


port txLogout : Bool -> Cmd msg


port txToken : String -> Cmd msg
