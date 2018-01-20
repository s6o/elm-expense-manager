module Manager.User
    exposing
        ( User(..)
        , UserField(..)
        , init
        , managerId
        )

import DRec exposing (DError, DRec, DType(..))


type User
    = User (DRec UserField)


type UserField
    = PkId
    | Email
    | Name
    | Lang


init : User
init =
    DRec.init
        |> DRec.field PkId DInt
        |> DRec.field Email DString
        |> DRec.field Name DString
        |> DRec.field Lang DString
        |> User


managerId : User -> Int
managerId (User drec) =
    DRec.get PkId drec
        |> DRec.toInt
        |> Result.withDefault 0
