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
import Manager.Account as Account exposing (Account(..))
import Manager.Jwt as Jwt
import Meld exposing (Error(..), Meld)
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


save : Int -> Meld Model Error Msg -> Task Error (Meld Model Error Msg)
save accountId meld =
    Account.validate accountId meld
        |> Task.andThen (patch accountId)
        |> Task.map
            (\pmeld ->
                let
                    model =
                        Meld.model pmeld

                    taskModel ma =
                        { ma
                            | accounts = model.accounts
                            , messages = Just "Saved."
                        }
                in
                Meld.withMerge taskModel pmeld
            )


get : Meld Model Error Msg -> Task Error (List Account)
get meld =
    let
        model =
            Meld.model meld

        (Account drec) =
            Account.init
    in
    model.apiBaseUrl
        ++ ("/accounts?mgr_id=eq." ++ (Basics.toString <| Jwt.userId model.claims))
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect
            (Json.Decode.list (DRec.decoder drec |> Json.Decode.map Account)
                |> Http.expectJson
            )
        |> HttpBuilder.toTask
        |> Task.mapError Meld.EHttp


patch : Int -> Meld Model Error Msg -> Task Error (Meld Model Error Msg)
patch accountId meld =
    let
        model =
            Meld.model meld

        fail msg =
            msg
                |> EMsg
                |> Task.fail
    in
    Dict.get accountId model.accounts
        |> Maybe.map
            (\(Account drec) ->
                model.apiBaseUrl
                    ++ ("/accounts?aid=eq." ++ (accountId |> Basics.toString))
                    |> HttpBuilder.patch
                    |> withHeaders (tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect Http.expectString
                    |> HttpBuilder.toTask
                    |> Task.mapError Meld.EHttp
                    |> Task.map (\_ -> meld)
            )
        |> Maybe.withDefault (fail <| "Incorrect account id: " ++ Basics.toString accountId)
