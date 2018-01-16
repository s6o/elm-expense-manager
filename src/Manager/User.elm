module Manager.User
    exposing
        ( User(..)
        , init
        , uid
        )

import DRec exposing (DError, DRec, DType(..))
import Manager.Jwt exposing (Jwt)


type User
    = User DRec


init : User
init =
    DRec.init
        |> DRec.field "mid" DInt
        |> DRec.field "email" DString
        |> DRec.field "name" DString
        |> DRec.field "lang" DString
        |> User


uid : User -> Int
uid (User drec) =
    DRec.get "uid" drec
        |> DRec.toInt
        |> Result.withDefault 0
