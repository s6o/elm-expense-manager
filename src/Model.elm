module Model
    exposing
        ( Model
        )

import Dict exposing (Dict)
import Manager.Auth exposing (AuthManager)
import Material
import Route exposing (Route, Tab)


type alias Model =
    { actions : Int
    , apiBaseUrl : String
    , errors : Maybe String
    , loading : Int
    , mdl : Material.Model
    , route : Route
    , tabs : Dict String Tab
    , token : Maybe String
    , authMgr : Maybe AuthManager
    }
