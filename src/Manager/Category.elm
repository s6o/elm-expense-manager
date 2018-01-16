module Manager.Category
    exposing
        ( Category(..)
        , init
        , parentPath
        )

import DRec exposing (DError, DRec, DType(..))


type Category
    = Category DRec


init : Category
init =
    DRec.init
        |> DRec.field "cid" DInt
        |> DRec.field "mgr_id" DInt
        |> DRec.field "name" DString
        |> DRec.field "parent_path" DString
        |> Category


parentPath : Category -> String
parentPath (Category drec) =
    DRec.get "parent_path" drec
        |> DRec.toString
        |> Result.withDefault ""
