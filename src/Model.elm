module Model
    exposing
        ( Model
        )

import Manager.Auth exposing (AuthManager)
import Time exposing (Time)


type alias Model =
    { actions : Int
    , apiBaseUrl : String
    , loading : Int
    , systemTick : Time
    , token : Maybe String
    , authMgr : Maybe AuthManager
    }
