module Api.Currency
    exposing
        ( read
        , save
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
import Json.Decode
import Manager.Currency as Currency exposing (Currency(..))
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


read : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
read meld =
    get meld
        |> Task.map
            (\c ->
                let
                    taskModel ma =
                        { ma | currency = c }
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


get : Meld Model Error Msg -> Task Error Currency
get meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency
    in
    model.apiBaseUrl
        ++ "/currency"
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson (DRec.decoder drec |> Json.Decode.map Currency))
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp


patch : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
patch meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency
    in
    model.apiBaseUrl
        ++ ("/currency?iso_code=eq." ++ Currency.isoCode model.currency)
        |> HttpBuilder.patch
        |> withHeaders (tokenHeader model.token)
        |> withJsonBody (DRec.encoder drec)
        |> withExpect Http.expectString
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp
        |> Task.map (\_ -> meld)
