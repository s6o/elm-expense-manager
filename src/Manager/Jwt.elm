module Manager.Jwt
    exposing
        ( init
        )

{-| Check JWT token payload/claims and extract them into a DRec if present.

Some token processing details are slighly refactored from elm-jwt:
<https://github.com/simonh1000/elm-jwt>

-}

import Base64
import DRec exposing (DError, DRec, DType(..))


{-| @private
Claims as constructed by PostgREST
-}
claims : DRec
claims =
    DRec.init
        |> DRec.field "role" DString
        |> DRec.field "uid" DInt
        |> DRec.field "email" DString
        |> DRec.field "exp" DInt


{-| Initialize a `DRec` of JWT claims.
In case of an empty token an empty `DRec` is returned.
-}
init : Maybe String -> DRec
init mtoken =
    mtoken
        |> Maybe.map
            (\t ->
                let
                    json =
                        case getTokenBody t of
                            Err msg ->
                                "{}"

                            Ok body ->
                                Base64.decode body
                                    |> Result.withDefault "{}"
                in
                DRec.decodeString claims json
                    |> Result.withDefault claims
            )
        |> Maybe.withDefault claims


{-| @private
-}
getTokenBody : String -> Result String String
getTokenBody token =
    let
        processor =
            unurl >> String.split "." >> List.map fixlength
    in
    case processor token of
        _ :: (Result.Err e) :: _ :: [] ->
            Result.Err e

        _ :: (Result.Ok encBody) :: _ :: [] ->
            Result.Ok encBody

        _ ->
            Result.Err "Token has invalid shape"


{-| @private
-}
unurl : String -> String
unurl =
    let
        fix c =
            case c of
                '-' ->
                    '+'

                '_' ->
                    '/'

                _ ->
                    c
    in
    String.map fix


{-| @private
-}
fixlength : String -> Result String String
fixlength s =
    case String.length s % 4 of
        0 ->
            Result.Ok s

        2 ->
            Result.Ok <| String.concat [ s, "==" ]

        3 ->
            Result.Ok <| String.concat [ s, "=" ]

        _ ->
            Result.Err "Wrong length"
