module Model
    exposing
        ( Flags
        , Model
        )

import Dict exposing (Dict)
import Manager.Account exposing (Account)
import Manager.Auth exposing (Auth)
import Manager.Category exposing (CategoryManagement)
import Manager.Currency exposing (Currency)
import Manager.Jwt exposing (Jwt)
import Manager.User exposing (User)
import Material
import Route exposing (Route, Tab)


type alias Flags =
    { token : Maybe String
    }


type alias Model =
    { apiBaseUrl : String
    , errors : Maybe String
    , messages : Maybe String
    , mdl : Material.Model
    , route : Route
    , tabs : Dict String Tab
    , token : Maybe String
    , auth : Auth
    , claims : Jwt
    , user : User
    , currency : Currency
    , accounts : Dict Int Account
    , category : Maybe CategoryManagement
    }
