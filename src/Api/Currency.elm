module Api.Currency
    exposing
        ( read
        , save
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
import Manager.Currency as MCurrency
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
                        { ma | currencyMgr = Ok drec }
                in
                Meld.withMerge taskModel meld
            )


save : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
save meld =
    MCurrency.validate meld
        |> Task.andThen patch
        |> Task.map
            (\res ->
                let
                    taskModel ma =
                        ma
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
        ++ "/currency"
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson (DRec.decoder model.currencyMgr))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp


patch : Meld Model Error Msg -> Task Error String
patch meld =
    let
        model =
            Meld.model meld

        cid =
            DRec.get "iso_code" model.currencyMgr
                |> DRec.toString
                |> Result.withDefault ""
    in
    model.apiBaseUrl
        ++ "/currency?iso_code=eq."
        ++ cid
        |> HttpBuilder.patch
        |> withHeaders (tokenHeader model.token)
        |> withJsonBody (DRec.toObject model.currencyMgr)
        |> withExpect Http.expectString
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
