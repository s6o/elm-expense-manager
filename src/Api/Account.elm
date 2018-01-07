module Api.Account
    exposing
        ( read
        )

import Api.Headers exposing (tokenHeader)
import DRec exposing (DError, DRec)
import Dict
import Http
import HttpBuilder exposing (..)
import Json.Decode
import Manager.Account as Account
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
                            | accounts =
                                results
                                    |> List.map (\drec -> ( Account.id (Ok drec), Ok drec ))
                                    |> Dict.fromList
                        }
                in
                Meld.withMerge taskModel meld
            )


get : Meld Model Error Msg -> Task Error (List DRec)
get meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ ("/accounts?mgr_id=eq." ++ (Basics.toString <| User.uid model))
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect (Http.expectJson <| Json.Decode.list (DRec.decoder Account.init))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
