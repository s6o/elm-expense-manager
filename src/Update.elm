module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Messages exposing (Msg)
import Model exposing (Model)


init : ( Model, Cmd Msg )
init =
    ( { apiBaseUrl = ""
      , loading = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
