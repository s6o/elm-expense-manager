module Manager.Auth
    exposing
        ( Auth(..)
        , AuthField(..)
        , JwtToken(..)
        , TokenFields(..)
        , email
        , fieldInput
        , init
        , jwtToken
        , pass
        , token
        , validate
        )

import DRec exposing (DError, DRec, DType(..))
import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:))
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type JwtToken
    = JwtToken (DRec TokenFields)


type TokenFields
    = Token


jwtToken : JwtToken
jwtToken =
    DRec.init
        |> DRec.field Token DString
        |> JwtToken


token : JwtToken -> Maybe String
token (JwtToken drec) =
    DRec.get Token drec
        |> DRec.toString
        |> Result.toMaybe


type alias Parent m =
    { m | auth : Auth }


type Auth
    = Auth (DRec AuthField)


type AuthField
    = Email
    | Pass


init : Auth
init =
    DRec.init
        |> DRec.field Email DString
        |> DRec.field Pass DString
        |> Auth


email : Auth -> String
email (Auth drec) =
    DRec.get Email drec
        |> DRec.toString
        |> Result.withDefault ""


pass : Auth -> String
pass (Auth drec) =
    DRec.get Pass drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : AuthField -> String -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
fieldInput field value meld =
    let
        model =
            Meld.model meld

        (Auth drec) =
            model.auth

        taskModel ma =
            { ma | auth = DRec.setString field value drec |> Auth }
    in
    Meld.withMerge taskModel meld
        |> Task.succeed


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
