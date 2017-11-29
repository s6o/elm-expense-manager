module Api
    exposing
        ( call
        , callSequence
        )

{-| Application Api for communicating with backend services.
Simple wrapper around `Meld` with customized error type.
-}

import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


{-| Execute API request `Task`s in any order.

For guaranteed dependent successive execution of request tasks use `callSequence`.

-}
call :
    Model
    -> List (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    -> ( Model, Cmd Msg )
call model tasks =
    Meld.init model
        |> Meld.withTasks tasks
        |> Meld.send
            Messages.Requests
            (\_ -> model.loading)
            (\tc -> { model | loading = tc })


{-| Execute backend API request tasks in sequence.

The request tasks are executed in sequence by calling the next task if the
previous one completed successfully.

Updates to `Model` and request `Task`'s `Cmd msg`s are aggregated.

-}
callSequence :
    Model
    -> List (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    -> ( Model, Cmd Msg )
callSequence model tasks =
    Meld.init model
        |> Meld.withTasks tasks
        |> Meld.sequence
            Messages.Requests
            (\_ -> model.loading)
            (\tc -> { model | loading = tc })
