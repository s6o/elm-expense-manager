module Manager.User
    exposing
        ( init
        )

import DRec exposing (DError, DRec, DType(..))


init : Result DError DRec
init =
    DRec.empty
        |> DRec.field "mid" DInt
        |> DRec.field "email" DString
        |> DRec.field "name" DString
        |> DRec.field "lang" DString
