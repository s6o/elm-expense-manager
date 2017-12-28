module Messages
    exposing
        ( Msg(..)
        )

import Material
import Meld exposing (Error, Meld)
import Model exposing (Model)
import Task exposing (Task)


{-| Messages for Elm's runtime.

    * Act - feed user actions to Elm's runtime in undetermined order
    * ActSeq - feed user actions to Elm's runtime sequentially only upon previous successes
    * IgnoreKey - ignore a keypress due to not matching an expected key code
    * Mdl - forward elm-mdl library messages
    * Requests - feed HTTP requests to Elm's runtime in undetermined order
    * Responses - process results from `Requests`
    * Results - process results from `Act` and `ActSeq`
    * SelectTab - main tab selection
    * TextInput - process text input from fields

-}
type Msg
    = Act (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | ActSeq (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | IgnoreKey
    | Mdl (Material.Msg Msg)
    | Request (List (Meld Model Error Msg -> Task Error (Meld Model Error Msg)))
    | Responses Int (Result Error (Meld Model Error Msg))
    | Results Int (Result Error (Meld Model Error Msg))
    | SelectTab String
    | TextInput (Model -> String -> ( Model, Cmd Msg )) Model String
