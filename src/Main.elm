module Main exposing (..)

import Html
import Messages exposing (Msg)
import Model exposing (Model)
import Update
import View


main : Program Never Model Msg
main =
    Html.program
        { init = Update.init
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
