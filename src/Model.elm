module Model
    exposing
        ( Model
        )

import Manager.Auth exposing (AuthManager)
import Material
import Route exposing (Route)


type alias Model =
    { actions : Int
    , apiBaseUrl : String
    , loading : Int
    , mdl : Material.Model
    , route : Route
    , token : Maybe String
    , authMgr : Maybe AuthManager
    }
