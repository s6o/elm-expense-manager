module Manager.Category
    exposing
        ( Category(..)
        , CategoryField(..)
        , init
        , parentPath
        )

import DRec exposing (DError, DRec, DType(..))


type Category
    = Category (DRec CategoryField)


type CategoryField
    = PkId
    | MgrId
    | Name
    | ParentPath


init : Category
init =
    DRec.init
        |> DRec.field PkId DInt
        |> DRec.field MgrId DInt
        |> DRec.field Name DString
        |> DRec.field ParentPath DString
        |> Category


parentPath : Category -> String
parentPath (Category drec) =
    DRec.get ParentPath drec
        |> DRec.toString
        |> Result.withDefault ""
