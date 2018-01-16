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
import Manager.Auth as Auth exposing (Auth(..), Token)
import Manager.Jwt as MJwt
import Meld exposing (Error, Meld)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Ports
import Route exposing (Route(..))
import Task exposing (Task)


login : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
login meld =
    Auth.validate meld
        |> Task.andThen postCredentials
        |> Task.map
            (\result ->
                let
                    taskModel ma =
                        let
                            (Auth drec) =
                                ma.auth
                        in
                        { ma
                            | route = Route.Transactions
                            , token = Just result.token
                            , auth = DRec.clear drec |> Auth
                            , claims = MJwt.init (Just result.token)
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
                        let
                            (Auth drec) =
                                ma.auth
                        in
                        { ma
                            | route = Route.Login
                            , token = Nothing
                            , auth = DRec.clear drec |> Auth
                            , claims = DRec.clear ma.claims
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

        (Auth drec) =
            model.auth
    in
    if DRec.isEmpty drec then
        "Model's `auth` member is not set."
            |> Meld.EMsg
            |> Task.fail
    else
        model.apiBaseUrl
            ++ "/rpc/login"
            |> HttpBuilder.post
            |> withHeaders objectHeader
            |> withJsonBody (DRec.encoder drec)
            |> withExpect (Http.expectJson Auth.decoder)
            |> HttpBuilder.toTask
            |> Task.mapError Meld.EHttp
