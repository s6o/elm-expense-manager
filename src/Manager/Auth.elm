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
    { m | authMgr : Result DError DRec }


type alias Token =
    { token : String }


decoder : Decoder Token
decoder =
    Json.Decode.succeed Token
        |: field "token" Json.Decode.string


init : Result DError DRec
init =
    DRec.empty
        |> DRec.field "email" DString
        |> DRec.field "pass" DString


fieldInput : String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput field model value =
    case
        DRec.setString field value model.authMgr
    of
        Err _ ->
            ( model
            , Cmd.none
            )

        Ok drec ->
            ( { model | authMgr = Ok drec }
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
    if DRec.isEmpty model.authMgr then
        credsFail
    else
        let
            email =
                DRec.get "email" model.authMgr
                    |> DRec.toString
                    |> Result.withDefault ""

            pass =
                DRec.get "pass" model.authMgr
                    |> DRec.toString
                    |> Result.withDefault ""
        in
        if String.length email > 0 && String.length pass > 0 then
            Task.succeed meld
        else
            credsFail
