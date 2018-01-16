module Model
    exposing
        ( Flags
        , Model
        )

import DRec exposing (DError, DRec)
import Dict exposing (Dict)
import Manager.Auth exposing (Auth)
import Manager.Category exposing (Category)
import Manager.Currency exposing (Currency)
import Manager.Jwt exposing (Jwt)
import Manager.User exposing (User)
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
    , auth : Auth
    , claims : Jwt
    , user : User
    , currency : Currency
    , accounts : Dict Int DRec
    , categories : Dict String (List Category)
    }
