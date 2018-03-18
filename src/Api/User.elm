module Api.User
    exposing
        ( read
        )

import Api.Headers exposing (objectHeader, tokenHeader)
import DRec exposing (DRec)
import Http
import HttpBuilder exposing (..)
import Json.Decode
import Manager.Jwt as Jwt
import Manager.User as User exposing (User(..))
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


read : Meld Model Msg -> Task (Error Model) (Meld Model Msg)
read meld =
    get meld
        |> Task.map
            (\u ->
                let
                    taskModel ma =
                        { ma | user = u }
                in
                Meld.withMerge taskModel meld
            )


get : Meld Model Msg -> Task (Error Model) User
get meld =
    let
        model =
            Meld.model meld

        (User drec) =
            model.user
    in
    model.apiBaseUrl
        ++ ("/managers?pk_id=eq." ++ (Basics.toString <| Jwt.userId model.claims))
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson (DRec.decoder drec |> Json.Decode.map User))
        |> HttpBuilder.toTask
        |> Task.mapError (Meld.EHttp model)
