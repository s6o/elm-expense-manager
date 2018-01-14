module Api.Account
    exposing
        ( read
        , save
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
                                    |> List.map (\drec -> ( Account.id drec, drec ))
                                    |> Dict.fromList
                        }
                in
                Meld.withMerge taskModel meld
            )


save : DRec -> Meld Model Error Msg -> Task Error (Meld Model Error Msg)
save drec meld =
    Account.validate drec meld
        |> Task.andThen (patch drec)
        |> Task.map
            (\res ->
                let
                    taskModel ma =
                        ma
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


patch : DRec -> Meld Model Error Msg -> Task Error String
patch drec meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ ("/accounts?aid=eq." ++ (Account.id drec |> Basics.toString))
        |> HttpBuilder.patch
        |> withHeaders (tokenHeader model.token)
        |> withJsonBody (DRec.encoder drec)
        |> withExpect Http.expectString
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
