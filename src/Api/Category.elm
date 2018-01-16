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
import Manager.Category as Category exposing (Category(..))
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


toTree : Category -> List ( String, List Category ) -> List ( String, List Category )
toTree (Category drec) subs =
    case subs of
        [] ->
            ( Category.parentPath (Category drec)
            , [ Category drec ]
            )
                :: subs

        ( spath, slist ) :: rest ->
            let
                cpath =
                    Category.parentPath (Category drec)
            in
            if cpath == spath then
                ( cpath, Category drec :: slist ) :: rest
            else
                ( cpath, Category drec :: [] ) :: (( spath, slist ) :: rest)


get : Meld Model Error Msg -> Task Error (List Category)
get meld =
    let
        model =
            Meld.model meld

        (Category drec) =
            Category.init
    in
    model.apiBaseUrl
        ++ ("/categories?mgr_id=eq." ++ (Basics.toString <| User.uid model.user) ++ "&order=parent_path.asc")
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect (Http.expectJson <| Json.Decode.list (DRec.decoder drec |> Json.Decode.map Category))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
