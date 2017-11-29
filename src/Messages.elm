module Messages
    exposing
        ( Msg(..)
        )

import Meld exposing (Error, Meld)
import Model exposing (Model)
import Task exposing (Task)
import Time exposing (Time)


type Msg
    = Act (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    | ActSeq (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    | Actions Int (Result Error (Meld Model Error Msg))
    | Requests Int (Result Error (Meld Model Error Msg))
    | SystemTick Time
    | TextInput (Model -> String -> ( Model, Cmd Msg )) Model String
    | Login
