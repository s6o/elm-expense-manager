module Api.User
    exposing
        ( read
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
import Manager.User as User
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


read : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
read meld =
    get meld
        |> Task.map
            (\drec ->
                let
                    taskModel ma =
                        { ma | user = drec }
                in
                Meld.withMerge taskModel meld
            )


get : Meld Model Error Msg -> Task Error DRec
get meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ ("/managers?mid=eq." ++ (Basics.toString <| User.uid model))
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson (DRec.decoder model.user))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
