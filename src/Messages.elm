module Messages
    exposing
        ( Msg(..)
        )

import Material
import Meld exposing (Error, Meld)
import Model exposing (Model)
import Task exposing (Task)


type Msg
    = Act (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    | ActSeq (Meld Model Error Msg -> Task Error (Meld Model Error Msg))
    | Actions Int (Result Error (Meld Model Error Msg))
    | Mdl (Material.Msg Msg)
    | Requests Int (Result Error (Meld Model Error Msg))
    | SelectTab Int
    | TextInput (Model -> String -> ( Model, Cmd Msg )) Model String
