module Api.Auth
    exposing
        ( login
        )

import Http
import HttpBuilder exposing (..)
import Manager.Auth exposing (Token)
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


login : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
login meld =
    Manager.Auth.validate meld
        |> Task.andThen postCredentials
        |> Task.map
            (\result ->
                let
                    taskModel ma =
                        { ma | token = Just result.token }
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
