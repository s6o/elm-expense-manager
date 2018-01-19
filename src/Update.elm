module Update
    exposing
        ( delta2url
        , init
        , subscriptions
        , update
        , url2messages
        )

import Api.Auth
import Api.Init exposing (initialRequests)
import Api.Response
import Dict
import Manager.Auth as Auth
import Manager.Currency as Currency
import Manager.Jwt as Jwt
import Manager.User as User
import Material
import Material.Layout as Layout
import Meld exposing (Error(..))
import Messages exposing (Msg(..))
import Model exposing (Flags, Model)
import Navigation exposing (Location)
import Route exposing (Tab, initTabs)
import RouteUrl exposing (UrlChange)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { apiBaseUrl = "/api"
            , errors = Nothing
            , messages = Nothing
            , mdl = Material.model
            , route = Route.Empty
            , tabs = initTabs
            , token = flags.token
            , auth = Auth.init
            , claims = Jwt.init flags.token
            , user = User.init
            , currency = Currency.init
            , accounts = Dict.empty
            , categories = Dict.empty
            }
    in
    ( model
    , Cmd.batch
        [ Layout.sub0 Mdl
        , initialRequests model
        ]
    )


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    if previous.route == current.route then
        { entry = RouteUrl.ModifyEntry
        , url = Route.toFragment current.route
        }
            |> Just
    else if previous.route /= current.route then
        { entry = RouteUrl.NewEntry
        , url = Route.toFragment current.route
        }
            |> Just
    else
        Nothing


url2messages : Location -> List Msg
url2messages location =
    [ SelectTab location.hash
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Act tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Results

        ActSeq tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.sequence Results

        IgnoreKey ->
            ( model, Cmd.none )

        Mdl mm ->
            Material.update Mdl mm model

        Request tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Responses

        Responses result ->
            case result of
                Ok meld ->
                    Meld.update model meld

                Err meldError ->
                    let
                        _ =
                            Debug.log "Request Error" meldError
                    in
                    ( { model
                        | errors =
                            Api.Response.errorMessage meldError
                                |> Maybe.withDefault (Meld.errorMessage meldError)
                                |> Just
                        , messages = Nothing
                      }
                    , case
                        Api.Response.isUnauthorized meldError
                            || Api.Response.isServerError meldError
                      of
                        False ->
                            Cmd.none

                        True ->
                            Meld.init model
                                |> Meld.addTasks [ Api.Auth.logout ]
                                |> Meld.cmds Responses
                    )

        Results result ->
            case result of
                Ok meld ->
                    Meld.update model meld

                Err meldError ->
                    let
                        _ =
                            Debug.log "Action Error" meldError
                    in
                    ( { model
                        | errors = Just <| Meld.errorMessage meldError
                        , messages = Nothing
                      }
                    , Cmd.none
                    )

        SelectTab fragment ->
            Route.tabs model.token model.tabs
                |> List.filter (\t -> fragment == Route.toFragment t.route)
                |> List.head
                |> Maybe.map
                    (\t ->
                        ( { model
                            | errors = Nothing
                            , messages = Nothing
                            , route = t.route
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault
                    ( { model
                        | errors = Nothing
                        , messages = Nothing
                        , route = Route.defaultRoute model.token
                      }
                    , Navigation.modifyUrl (Route.defaultRoute model.token |> Route.toFragment)
                    )

        TextInput task input ->
            Meld.init model
                |> Meld.addTasks [ task input ]
                |> Meld.send Results


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        ]
