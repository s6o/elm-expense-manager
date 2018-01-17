module Manager.User
    exposing
        ( User(..)
        , init
        , managerId
        )

import DRec exposing (DError, DRec, DType(..))


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


managerId : User -> Int
managerId (User drec) =
    DRec.get "mid" drec
        |> DRec.toInt
        |> Result.withDefault 0
