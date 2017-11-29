module Manager.Auth
    exposing
        ( AuthManager
        , Token
        , decoder
        , emailInput
        , encode
        , passInput
        , validate
        )

import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:))
import Json.Encode exposing (Value)
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type alias Parent m =
    { m | authMgr : Maybe AuthManager }


type alias AuthManager =
    { email : String
    , pass : String
    }


type alias Token =
    { token : String }


decoder : Decoder Token
decoder =
    Json.Decode.succeed Token
        |: field "token" Json.Decode.string


encode : AuthManager -> Value
encode r =
    Json.Encode.object
        [ ( "email", Json.Encode.string r.email )
        , ( "pass", Json.Encode.string r.pass )
        ]


emailInput : Parent m -> String -> ( Parent m, Cmd msg )
emailInput model value =
    ( { model
        | authMgr =
            model.authMgr
                |> Maybe.map (\r -> { r | email = value })
                |> Maybe.withDefault (AuthManager value "")
                |> Just
      }
    , Cmd.none
    )


passInput : Parent m -> String -> ( Parent m, Cmd msg )
passInput model value =
    ( { model
        | authMgr =
            model.authMgr
                |> Maybe.map (\r -> { r | pass = value })
                |> Maybe.withDefault (AuthManager "" value)
                |> Just
      }
    , Cmd.none
    )


validate : Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
validate meld =
    let
        model =
            Meld.model meld

        credsFail =
            "Authentication credentials not set."
                |> EMsg
                |> Task.fail
    in
    case model.authMgr of
        Nothing ->
            credsFail

        Just mgr ->
            if String.length mgr.email > 0 && String.length mgr.pass > 0 then
                Task.succeed meld
            else
                credsFail
