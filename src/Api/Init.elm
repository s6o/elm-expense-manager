module Api.Init
    exposing
        ( initialRequests
        )

import Api.Account
import Api.Category
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
                        , Api.Account.read
                        , Api.Category.read
                        ]
                in
                Task.perform
                    Act
                    (Task.succeed initialTasks)
            )
        |> Maybe.withDefault Cmd.none
