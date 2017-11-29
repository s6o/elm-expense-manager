module Ui
    exposing
        ( act
        , actSequence
        )

import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


{-| Execute UI action `Task`s in any order.

For guaranteed dependent successive execution of action tasks use `actSequence`.

-}
act :
    Model
    -> List (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    -> ( Model, Cmd Msg )
act model tasks =
    Meld.init model
        |> Meld.withTasks tasks
        |> Meld.send
            Messages.Actions
            (\_ -> model.actions)
            (\ac -> { model | actions = ac })


{-| Execute UI action tasks in sequence.

The action tasks are executed in sequence by calling the next task if the
previous one completed successfully.

Updates to `Model` and action `Task`'s `Cmd msg`s are aggregated.

-}
actSequence :
    Model
    -> List (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    -> ( Model, Cmd Msg )
actSequence model tasks =
    Meld.init model
        |> Meld.withTasks tasks
        |> Meld.sequence
            Messages.Actions
            (\_ -> model.actions)
            (\ac -> { model | actions = ac })
