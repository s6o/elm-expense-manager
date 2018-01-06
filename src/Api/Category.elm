module Api.Category
    exposing
        ( read
        )

import Api.Headers exposing (tokenHeader)
import DRec exposing (DError, DRec)
import Dict
import Http
import HttpBuilder exposing (..)
import Json.Decode
import Manager.Category as Category
import Manager.User as User
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


read : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
read meld =
    get meld
        |> Task.map
            (\results ->
                let
                    taskModel ma =
                        { ma
                            | categories =
                                results
                                    |> List.foldl toTree []
                                    |> Dict.fromList
                        }
                in
                Meld.withMerge taskModel meld
            )


toTree : DRec -> List ( String, List (Result DError DRec) ) -> List ( String, List (Result DError DRec) )
toTree drec subs =
    case subs of
        [] ->
            ( DRec.get "parent_path" (Ok drec)
                |> DRec.toString
                |> Result.withDefault ""
            , [ Ok drec ]
            )
                :: subs

        ( spath, slist ) :: rest ->
            let
                cpath =
                    DRec.get "parent_path" (Ok drec)
                        |> DRec.toString
                        |> Result.withDefault ""
            in
            if cpath == spath then
                ( cpath, Ok drec :: slist ) :: rest
            else
                ( cpath, Ok drec :: [] ) :: (( spath, slist ) :: rest)


get : Meld Model Error Msg -> Task Error (List DRec)
get meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ ("/categories?mgr_id=eq." ++ (Basics.toString <| User.uid model) ++ "&order=parent_path.asc")
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect (Http.expectJson <| Json.Decode.list (DRec.decoder Category.init))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
