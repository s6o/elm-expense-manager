module Manager.Account
    exposing
        ( init
        )

import DRec exposing (DError, DRec, DType(..))


init : Result DError DRec
init =
    DRec.empty
        |> DRec.field "aid" DInt
        |> DRec.field "mgr_id" DInt
        |> DRec.field "name" DString
        |> DRec.field "initial_balance" DInt
        |> DRec.field "bank_account" DString
        |> DRec.field "bank_name" DString
