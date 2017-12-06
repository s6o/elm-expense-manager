module Model
    exposing
        ( Model
        )

import Dict exposing (Dict)
import Manager.Auth exposing (AuthManager)
import Material
import Route exposing (Route)


type alias Model =
    { actions : Int
    , apiBaseUrl : String
    , loading : Int
    , mdl : Material.Model
    , route : Route
    , tabIndex : Int
    , tabs : Dict Int String
    , token : Maybe String
    , authMgr : Maybe AuthManager
    }
