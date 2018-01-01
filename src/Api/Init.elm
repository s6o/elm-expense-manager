module Api.Init
    exposing
        ( initialRequests
        )

import Api.Currency
import Api.User
import Messages exposing (Msg(..))
import Model exposing (Model)
import Task


{-| Dispatch initial request to initialize the `Model` after successful auth.
-}
initialRequests : Model -> Cmd Msg
initialRequests model =
    model.token
        |> Maybe.map
            (\t ->
                let
                    initialTasks =
                        [ Api.Currency.read
                        , Api.User.read
                        ]
                in
                Task.perform
                    Request
                    (Task.succeed initialTasks)
            )
        |> Maybe.withDefault Cmd.none
