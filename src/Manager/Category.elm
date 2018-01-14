module Manager.Category
    exposing
        ( init
        )

import DRec exposing (DError, DRec, DType(..))


init : DRec
init =
    DRec.init
        |> DRec.field "cid" DInt
        |> DRec.field "mgr_id" DInt
        |> DRec.field "name" DString
        |> DRec.field "parent_path" DString
