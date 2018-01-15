module Model
    exposing
        ( Flags
        , Model
        )

import DRec exposing (DError, DRec)
import Dict exposing (Dict)
import Material
import Route exposing (Route, Tab)


type alias Flags =
    { token : Maybe String
    }


type alias Model =
    { actions : Int
    , apiBaseUrl : String
    , errors : Maybe String
    , messages : Maybe String
    , loading : Int
    , mdl : Material.Model
    , route : Route
    , tabs : Dict String Tab
    , token : Maybe String
    , auth : DRec
    , claims : DRec
    , user : DRec
    , currency : DRec
    , accounts : Dict Int DRec
    , categories : Dict String (List DRec)
    }
