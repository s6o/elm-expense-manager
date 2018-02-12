module Api.Auth
    exposing
        ( login
        , logout
        )

import Api.Http as AHttp exposing (ApiPath(..))
import Api.Init exposing (initialRequests)
import DRec
import Json.Decode
import Manager.Auth as Auth exposing (Auth(..), JwtToken(..))
import Manager.Jwt as Jwt exposing (Jwt(..))
import Meld exposing (Error, Meld)
import Messages exposing (Msg(..))
import Model exposing (Model)
import Ports
import Route exposing (Route(..))
import Task exposing (Task)


login : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
login meld =
    let
        model =
            Meld.model meld

        (Auth drec) =
            model.auth

        (JwtToken trec) =
            Auth.jwtToken

        tokenDecoder =
            DRec.decoder trec |> Json.Decode.map JwtToken

        drecFn f m =
            f m |> (\(Auth drec) -> Just drec)
    in
    if DRec.isEmpty drec then
        "Model's `auth` member is not set."
            |> Meld.EMsg
            |> Task.fail
    else
        Auth.validate meld
            |> Task.andThen (AHttp.postMap (ApiPath "/rpc/login") (drecFn .auth) tokenDecoder)
            |> Task.map
                (\jwtToken ->
                    let
                        taskModel ma =
                            let
                                (Auth adrec) =
                                    ma.auth
                            in
                            { ma
                                | route = Route.Transactions
                                , token = Auth.token jwtToken
                                , auth = DRec.clear adrec |> Auth
                                , claims = Auth.token jwtToken |> Jwt.init
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

                            (Jwt jrec) =
                                ma.claims
                        in
                        { ma
                            | route = Route.Login
                            , token = Nothing
                            , auth = DRec.clear drec |> Auth
                            , claims = DRec.clear jrec |> Jwt
                        }

                    clearStorage ma =
                        Ports.txLogout True
                in
                Meld.withMerge taskModel meld
                    |> Meld.withCmds [ clearStorage ]
            )
