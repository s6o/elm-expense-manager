module Manager.Category
    exposing
        ( Category(..)
        , CategoryField(..)
        , CategoryManagement
        , defaultId
        , empty
        , filterMain
        , id
        , init
        , name
        , nameInput
        , parentPath
        , sortWithName
        )

import DRec exposing (DError, DRec, DType(..))
import Dict exposing (Dict)
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type alias Parent m =
    { m
        | category : Maybe CategoryManagement
    }


type alias CategoryManagement =
    { items : Dict Int Category
    , marked : List Int
    , selected : Maybe Int
    }


type Category
    = Category (DRec CategoryField)


type CategoryField
    = PkId
    | MgrId
    | Name
    | ParentPath


defaultId : Int
defaultId =
    0


init : Category
init =
    DRec.init
        |> DRec.field PkId DInt
        |> DRec.field MgrId DInt
        |> DRec.field Name DString
        |> DRec.field ParentPath DString
        |> Category


empty : Int -> Category
empty managerId =
    init
        |> (\(Category drec) -> DRec.setInt MgrId managerId drec)
        |> Category


id : Category -> Int
id (Category drec) =
    DRec.get PkId drec
        |> DRec.toInt
        |> Result.withDefault 0


name : Category -> String
name (Category drec) =
    DRec.get Name drec
        |> DRec.toString
        |> Result.withDefault ""


parentPath : Category -> String
parentPath (Category drec) =
    DRec.get ParentPath drec
        |> DRec.toString
        |> Result.withDefault ""


nameInput : Int -> String -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
nameInput categoryId value meld =
    let
        model =
            Meld.model meld

        categories =
            model.category
                |> Maybe.map .items
                |> Maybe.withDefault Dict.empty
    in
    Dict.get categoryId categories
        |> Maybe.map
            (\(Category drec) ->
                let
                    newDRec =
                        DRec.setString Name value drec

                    taskModel ma =
                        { ma
                            | category =
                                ma.category
                                    |> Maybe.map
                                        (\r ->
                                            { r
                                                | items =
                                                    Dict.insert categoryId (Category newDRec) r.items
                                            }
                                        )
                        }
                in
                Meld.withMerge taskModel meld
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.succeed meld)


filterMain : Int -> Category -> Bool
filterMain _ c =
    if parentPath c == "/" then
        True
    else
        False


sortWithName : Category -> Category -> Order
sortWithName lft rgt =
    let
        lftName =
            name lft

        rgtName =
            name rgt
    in
    if lftName < rgtName then
        LT
    else if lftName > rgtName then
        GT
    else
        EQ
