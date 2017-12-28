module Api.Auth
    exposing
        ( login
        , logout
        )

import Http
import HttpBuilder exposing (..)
import Manager.Auth exposing (Token)
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Ports
import Route exposing (Route(..))
import Task exposing (Task)


login : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
login meld =
    Manager.Auth.validate meld
        |> Task.andThen postCredentials
        |> Task.map
            (\result ->
                let
                    taskModel ma =
                        { ma
                            | route = Route.Transactions
                            , token = Just result.token
                            , authMgr = Nothing
                        }

                    storeToken ma =
                        ma.token
                            |> Maybe.map Ports.txToken
                            |> Maybe.withDefault Cmd.none
                in
                Meld.withMerge taskModel meld
                    |> Meld.withCmds [ storeToken ]
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
                            , authMgr = Nothing
                        }
                in
                Meld.withMerge taskModel meld
            )


postCredentials : Meld Model Error Msg -> Task Error Token
postCredentials meld =
    let
        model =
            Meld.model meld
    in
    case model.authMgr of
        Nothing ->
            "Model's `authManager` is not set."
                |> Meld.EMsg
                |> Task.fail

        Just mgr ->
            let
                payload =
                    Manager.Auth.encode mgr
            in
            model.apiBaseUrl
                ++ "/rpc/login"
                |> HttpBuilder.post
                |> withHeaders
                    [ ( "Accept", "application/vnd.pgrst.object+json" )
                    , ( "X-Manager-Auth", "elm-expense-manager" )
                    ]
                |> withJsonBody payload
                |> withExpect (Http.expectJson Manager.Auth.decoder)
                |> HttpBuilder.toTask
                |> Task.mapError Meld.EHttp
