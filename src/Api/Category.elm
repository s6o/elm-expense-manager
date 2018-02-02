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
import Manager.Jwt as Jwt
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Set
import Task exposing (Task)


read : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
read meld =
    get meld
        |> Task.map
            (\results ->
                let
                    taskModel ma =
                        { ma
                            | category =
                                { items =
                                    results
                                        |> List.map (\c -> ( Category.id c, c ))
                                        |> Dict.fromList
                                , marked = Set.empty
                                , new =
                                    Category.empty (Jwt.userId ma.claims)
                                        |> Just
                                , selected = Nothing
                                , subselected = Nothing
                                }
                                    |> Just
                        }
                in
                Meld.withMerge taskModel meld
            )


get : Meld Model Error Msg -> Task Error (List Category)
get meld =
    let
        model =
            Meld.model meld

        (Category drec) =
            Category.init
    in
    model.apiBaseUrl
        ++ ("/categories?mgr_id=eq." ++ (Basics.toString <| Jwt.userId model.claims) ++ "&order=parent_path.asc")
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect (Http.expectJson <| Json.Decode.list (DRec.decoder drec |> Json.Decode.map Category))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
