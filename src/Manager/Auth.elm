module Manager.Auth
    exposing
        ( Token
        , decoder
        , fieldInput
        , init
        , validate
        )

import DRec exposing (DError, DRec, DType(..))
import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:))
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type alias Parent m =
    { m | auth : DRec }


type alias Token =
    { token : String }


decoder : Decoder Token
decoder =
    Json.Decode.succeed Token
        |: field "token" Json.Decode.string


init : DRec
init =
    DRec.init
        |> DRec.field "email" DString
        |> DRec.field "pass" DString


fieldInput : String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput field model value =
    ( { model | auth = DRec.setString field value model.auth }
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
    if DRec.isEmpty model.auth then
        credsFail
    else
        let
            email =
                DRec.get "email" model.auth
                    |> DRec.toString
                    |> Result.withDefault ""

            pass =
                DRec.get "pass" model.auth
                    |> DRec.toString
                    |> Result.withDefault ""
        in
        if String.length email > 0 && String.length pass > 0 then
            Task.succeed meld
        else
            credsFail
