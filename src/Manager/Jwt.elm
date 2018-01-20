module Manager.Jwt
    exposing
        ( Jwt(..)
        , JwtField(..)
        , init
        , userId
        )

{-| Check JWT token payload/claims and extract them into a DRec if present.

Some token processing details are slighly refactored from elm-jwt:
<https://github.com/simonh1000/elm-jwt>

-}

import Base64
import DRec exposing (DError, DRec, DType(..))


type Jwt
    = Jwt (DRec JwtField)


type JwtField
    = Role
    | Uid
    | Email
    | Exp


{-| @private
Claims as constructed by PostgREST
-}
claims : Jwt
claims =
    DRec.init
        |> DRec.field Role DString
        |> DRec.field Uid DInt
        |> DRec.field Email DString
        |> DRec.field Exp DInt
        |> Jwt


userId : Jwt -> Int
userId (Jwt drec) =
    DRec.get Uid drec
        |> DRec.toInt
        |> Result.withDefault 0


{-| Initialize a `DRec` of JWT claims.
In case of an empty token an empty `DRec` is returned.
-}
init : Maybe String -> Jwt
init mtoken =
    mtoken
        |> Maybe.map
            (\t ->
                let
                    (Jwt drec) =
                        claims

                    json =
                        case getTokenBody t of
                            Err msg ->
                                "{}"

                            Ok body ->
                                Base64.decode body
                                    |> Result.withDefault "{}"
                in
                DRec.decodeString drec json
                    |> Result.map Jwt
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
