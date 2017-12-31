module Api.Init
    exposing
        ( initialRequests
        )

import Api.Currency
import Meld exposing (Error, Meld)
import Messages exposing (Msg(..))
import Model exposing (Model)


{-| Dispatch initial request to initialize the `Model` after successful auth.
-}
initialRequests : Model -> Cmd Msg
initialRequests model =
    model.token
        |> Maybe.map
            (\t ->
                let
                    initialTasks =
                        [ Api.Currency.read ]
                in
                Meld.init { model | loading = model.loading + List.length initialTasks }
                    |> Meld.addTasks initialTasks
                    |> Meld.cmds (Responses 0)
             --<| List.length initialTasks)
            )
        |> Maybe.withDefault Cmd.none
