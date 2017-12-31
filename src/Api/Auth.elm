module Api.Auth
    exposing
        ( login
        , logout
        )

import Api.Headers exposing (objectHeader)
import Api.Init exposing (initialRequests)
import DRec
import Http
import HttpBuilder exposing (..)
import Manager.Auth as MAuth exposing (Token)
import Meld exposing (Error, Meld)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Ports
import Route exposing (Route(..))
import Task exposing (Task)


login : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
login meld =
    MAuth.validate meld
        |> Task.andThen postCredentials
        |> Task.map
            (\result ->
                let
                    taskModel ma =
                        { ma
                            | route = Route.Transactions
                            , token = Just result.token
                            , authMgr = DRec.clear ma.authMgr
                        }

                    storeToken ma =
                        ma.token
                            |> Maybe.map Ports.txToken
                            |> Maybe.withDefault Cmd.none
                in
                Meld.withMerge taskModel meld
                    |> Meld.withCmds [ storeToken, initialRequests ]
            )


logout : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
logout meld =
    Task.succeed meld
        |> Task.map
            (\_ ->
                let
                    taskModel ma =
                        { ma
                            | route = Route.Login
                            , token = Nothing
                            , authMgr = DRec.clear ma.authMgr
                        }

                    clearStorage ma =
                        Ports.txLogout True
                in
                Meld.withMerge taskModel meld
                    |> Meld.withCmds [ clearStorage ]
            )


postCredentials : Meld Model Error Msg -> Task Error Token
postCredentials meld =
    let
        model =
            Meld.model meld
    in
    if DRec.isEmpty model.authMgr then
        "Model's `authManager` is not set."
            |> Meld.EMsg
            |> Task.fail
    else
        let
            payload =
                DRec.toObject model.authMgr
        in
        model.apiBaseUrl
            ++ "/rpc/login"
            |> HttpBuilder.post
            |> withHeaders objectHeader
            |> withJsonBody payload
            |> withExpect (Http.expectJson MAuth.decoder)
            |> HttpBuilder.toTask
            |> Task.mapError Meld.EHttp
