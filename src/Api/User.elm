module Api.User
    exposing
        ( read
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
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
                        { ma | userMgr = Ok drec }
                in
                Meld.withMerge taskModel meld
            )


get : Meld Model Error Msg -> Task Error DRec
get meld =
    let
        model =
            Meld.model meld

        uid =
            DRec.get "uid" model.claimsMgr
                |> DRec.toInt
                |> Result.withDefault 0

        _ =
            Debug.log "claims" model.claimsMgr
    in
    model.apiBaseUrl
        ++ ("/managers?mid=eq." ++ Basics.toString uid)
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson (DRec.decoder model.userMgr))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
