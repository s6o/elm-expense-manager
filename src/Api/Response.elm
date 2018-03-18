module Api.Response
    exposing
        ( Error
        , errorMessage
        , isServerError
        , isUnauthorized
        )

import Http
import Json.Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:))
import Meld
import Result
import String.Extra as EStr


type alias Error =
    { hint : Maybe String
    , details : Maybe String
    , code : String
    , message : String
    }


decoder : Decoder Error
decoder =
    Json.Decode.succeed Error
        |: (Json.Decode.maybe <| field "hint" Json.Decode.string)
        |: (Json.Decode.maybe <| field "details" Json.Decode.string)
        |: field "code" Json.Decode.string
        |: field "message" Json.Decode.string


errorMessage : Meld.Error m -> Maybe String
errorMessage merr =
    case Meld.httpError merr of
        Nothing ->
            Nothing

        Just error ->
            case error of
                Http.BadStatus { body } ->
                    Json.Decode.decodeString decoder body
                        |> Result.map (.message >> EStr.toSentenceCase)
                        |> Result.toMaybe

                _ ->
                    Nothing


isServerError : Meld.Error m -> Bool
isServerError merr =
    case Meld.httpError merr of
        Nothing ->
            False

        Just error ->
            case error of
                Http.BadStatus { status } ->
                    status.code == 500

                _ ->
                    False


isUnauthorized : Meld.Error m -> Bool
isUnauthorized merr =
    case Meld.httpError merr of
        Nothing ->
            False

        Just error ->
            case error of
                Http.BadStatus { status } ->
                    status.code == 401

                _ ->
                    False
