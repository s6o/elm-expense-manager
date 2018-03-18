module Messages
    exposing
        ( Msg(..)
        )

import Material
import Meld exposing (Error, Meld)
import Model exposing (Model)
import Navigation exposing (Location)
import Task exposing (Task)


{-| Messages for Elm's runtime.

    * Act - feed user actions to Elm's runtime in undetermined order
    * ActSeq - feed user actions to Elm's runtime sequentially only upon previous successes
    * IgnoreKey - ignore a keypress due to not matching an expected key code
    * Mdl - forward elm-mdl library messages
    * Responses - process results from 'Act' and 'ActSeq' and 'TextInput'
    * SelectTab - main tab selection
    * TextInput - process text input from fields

-}
type Msg
    = Act (List (Meld Model Msg -> Task (Error Model) (Meld Model Msg)))
    | ActSeq (List (Meld Model Msg -> Task (Error Model) (Meld Model Msg)))
    | IgnoreKey
    | Mdl (Material.Msg Msg)
    | Responses (Result (Error Model) (Meld Model Msg))
    | SelectTab (Maybe Location)
    | TextInput (String -> Meld Model Msg -> Task (Error Model) (Meld Model Msg)) String
