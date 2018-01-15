module Api.Currency
    exposing
        ( read
        , save
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
import Manager.Currency as Currency
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
                        { ma | currency = drec }
                in
                Meld.withMerge taskModel meld
            )


save : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
save meld =
    Currency.validate meld
        |> Task.andThen patch
        |> Task.map
            (\pmeld ->
                let
                    model =
                        Meld.model pmeld

                    taskModel ma =
                        { ma
                            | messages = Just "Saved."
                            , currency = model.currency
                        }
                in
                Meld.withMerge taskModel pmeld
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
        |> withExpect (Http.expectJson (DRec.decoder model.currency))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp


patch : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
patch meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ ("/currency?iso_code=eq." ++ Currency.isoCode model.currency)
        |> HttpBuilder.patch
        |> withHeaders (tokenHeader model.token)
        |> withJsonBody (DRec.encoder model.currency)
        |> withExpect Http.expectString
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
        |> Task.map (\_ -> meld)
