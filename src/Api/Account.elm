module Api.Account
    exposing
        ( add
        , read
        , save
        )

import Api.Headers exposing (objectHeader, recordHeader, tokenHeader)
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


add : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
add meld =
    Account.validate Account.defaultId meld
        |> Task.andThen post
        |> Task.map
            (\account ->
                let
                    taskModel ma =
                        { ma
                            | messages = Just "Saved."
                            , accounts =
                                Dict.insert (Account.id account) account ma.accounts
                                    |> Dict.insert Account.defaultId (Account.empty <| Jwt.userId ma.claims)
                        }
                in
                Meld.withMerge taskModel meld
            )


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
                                    |> List.map (\a -> ( Account.id a, a ))
                                    |> (\accounts ->
                                            ( Account.defaultId
                                            , Account.empty <| Jwt.userId ma.claims
                                            )
                                                :: accounts
                                       )
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
                    ++ ("/accounts?pk_id=eq." ++ (accountId |> Basics.toString))
                    |> HttpBuilder.patch
                    |> withHeaders (tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect Http.expectString
                    |> HttpBuilder.toTask
                    |> Task.mapError Meld.EHttp
                    |> Task.map (\_ -> meld)
            )
        |> Maybe.withDefault (fail <| "Incorrect account id: " ++ Basics.toString accountId)


post : Meld Model Error Msg -> Task Error Account
post meld =
    let
        model =
            Meld.model meld

        fail msg =
            msg
                |> EMsg
                |> Task.fail
    in
    Dict.get Account.defaultId model.accounts
        |> Maybe.map
            (\(Account drec) ->
                model.apiBaseUrl
                    ++ "/accounts"
                    |> HttpBuilder.post
                    |> withHeaders (objectHeader ++ recordHeader ++ tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect
                        (DRec.decoder drec
                            |> Json.Decode.map Account
                            |> Http.expectJson
                        )
                    |> HttpBuilder.toTask
                    |> Task.mapError Meld.EHttp
            )
        |> Maybe.withDefault (fail <| "No new account record.")
