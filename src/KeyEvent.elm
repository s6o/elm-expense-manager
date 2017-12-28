module KeyEvent
    exposing
        ( onEnter
        )

import Html.Events exposing (keyCode, on)
import Json.Decode
import Material.Options as Options exposing (Property)
import Messages exposing (Msg(..))


onEnter : Msg -> Property c Msg
onEnter msg =
    Options.on "keydown" (Json.Decode.map (keyCodeMatch 13 msg) keyCode)


keyCodeMatch : Int -> Msg -> Int -> Msg
keyCodeMatch expected msg pressed =
    if expected == pressed then
        msg
    else
        IgnoreKey
