module Manager.Category
    exposing
        ( Category(..)
        , CategoryField(..)
        , CategoryManagement
        , NameAction(..)
        , defaultId
        , empty
        , filterMain
        , filterSub
        , id
        , init
        , name
        , nameInput
        , parentPath
        , select
        , sortWithName
        , toggle
        , unselect
        )

import DRec exposing (DError, DRec, DType(..))
import Dict exposing (Dict)
import Maybe.Extra as EMaybe
import Meld exposing (Error(..), Meld)
import Set exposing (Set)
import Task exposing (Task)


type alias Parent m =
    { m
        | category : Maybe CategoryManagement
    }


type alias CategoryManagement =
    { items : Dict Int Category
    , marked : Set Int
    , new : Maybe Category
    , selected : Maybe Category
    , subselected : Maybe Category
    }


type Category
    = Category (DRec CategoryField)


type CategoryField
    = PkId
    | MgrId
    | Name
    | ParentPath


type NameAction
    = New
    | Edit


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
        |> (\(Category drec) ->
                DRec.setInt PkId defaultId drec
                    |> DRec.setInt MgrId managerId
           )
        |> Category


id : Category -> Int
id (Category drec) =
    DRec.get PkId drec
        |> DRec.toInt
        |> Result.withDefault defaultId


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


nameInput : NameAction -> String -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
nameInput action value meld =
    let
        model =
            Meld.model meld

        category =
            model.category
                |> Maybe.map
                    (\r ->
                        case action of
                            New ->
                                r.new

                            Edit ->
                                case r.subselected of
                                    Nothing ->
                                        r.selected

                                    Just _ ->
                                        r.subselected
                    )
                |> EMaybe.join
    in
    category
        |> Maybe.map
            (\(Category drec) ->
                let
                    newDRec =
                        DRec.setString Name value drec

                    taskModel ma =
                        case action of
                            New ->
                                { ma
                                    | category =
                                        ma.category
                                            |> Maybe.map (\r -> { r | new = Just (Category newDRec) })
                                }

                            Edit ->
                                { ma
                                    | category =
                                        ma.category
                                            |> Maybe.map
                                                (\r ->
                                                    case r.subselected of
                                                        Nothing ->
                                                            { r | selected = Just (Category newDRec) }

                                                        Just _ ->
                                                            { r | subselected = Just (Category newDRec) }
                                                )
                                }
                in
                Meld.withMerge taskModel meld
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.succeed meld)


select : Int -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
select categoryId meld =
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
            (\c ->
                let
                    taskModel ma =
                        { ma
                            | category =
                                ma.category
                                    |> Maybe.map
                                        (\r ->
                                            case r.selected of
                                                Nothing ->
                                                    { r | selected = Just c }

                                                Just _ ->
                                                    { r | subselected = Just c }
                                        )
                        }
                in
                Meld.withMerge taskModel meld
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.succeed meld)


toggle : Int -> Bool -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
toggle categoryId value meld =
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
            (\category ->
                let
                    taskModel ma =
                        { ma
                            | category =
                                ma.category
                                    |> Maybe.map
                                        (\r ->
                                            { r
                                                | marked =
                                                    if value then
                                                        Set.insert (id category) r.marked
                                                    else
                                                        Set.remove (id category) r.marked
                                            }
                                        )
                        }
                in
                Meld.withMerge taskModel meld
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.succeed meld)


unselect : Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
unselect meld =
    let
        model =
            Meld.model meld
    in
    model.category
        |> Maybe.map
            (\_ ->
                let
                    taskModel ma =
                        { ma
                            | category =
                                ma.category
                                    |> Maybe.map
                                        (\r ->
                                            case r.subselected of
                                                Nothing ->
                                                    { r | selected = Nothing }

                                                Just _ ->
                                                    { r | subselected = Nothing }
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


filterSub : String -> Int -> Category -> Bool
filterSub main _ c =
    let
        searchPath =
            "/" ++ main
    in
    if searchPath == parentPath c then
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
