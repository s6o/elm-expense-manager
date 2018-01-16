module Manager.Auth
    exposing
        ( Auth(..)
        , Token
        , decoder
        , email
        , fieldInput
        , init
        , pass
        , validate
        )

import DRec exposing (DError, DRec, DType(..))
import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:))
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type alias Parent m =
    { m | auth : Auth }


type Auth
    = Auth DRec


type alias Token =
    { token : String }


decoder : Decoder Token
decoder =
    Json.Decode.succeed Token
        |: field "token" Json.Decode.string


init : Auth
init =
    DRec.init
        |> DRec.field "email" DString
        |> DRec.field "pass" DString
        |> Auth


email : Auth -> String
email (Auth drec) =
    DRec.get "email" drec
        |> DRec.toString
        |> Result.withDefault ""


pass : Auth -> String
pass (Auth drec) =
    DRec.get "pass" drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput field model value =
    let
        (Auth drec) =
            model.auth
    in
    ( { model | auth = DRec.setString field value drec |> Auth }
    , Cmd.none
    )


validate : Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
validate meld =
    let
        model =
            Meld.model meld

        (Auth drec) =
            model.auth

        credsFail =
            "Authentication credentials not set."
                |> EMsg
                |> Task.fail
    in
    if DRec.isEmpty drec then
        credsFail
    else if String.length (email model.auth) > 0 && String.length (pass model.auth) > 0 then
        Task.succeed meld
    else
        credsFail
