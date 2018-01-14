module Manager.User
    exposing
        ( init
        , uid
        )

import DRec exposing (DError, DRec, DType(..))


type alias Parent m =
    { m
        | claims : DRec
        , user : DRec
    }


init : DRec
init =
    DRec.init
        |> DRec.field "mid" DInt
        |> DRec.field "email" DString
        |> DRec.field "name" DString
        |> DRec.field "lang" DString


uid : Parent m -> Int
uid model =
    DRec.get "uid" model.claims
        |> DRec.toInt
        |> Result.withDefault 0
