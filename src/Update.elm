module Update
    exposing
        ( delta2url
        , init
        , subscriptions
        , update
        , url2messages
        )

import Material
import Material.Layout as Layout
import Meld exposing (Error(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
import Navigation exposing (Location)
import Route exposing (Tab, initTabs)
import RouteUrl exposing (UrlChange)


init : ( Model, Cmd Msg )
init =
    ( { actions = 0
      , apiBaseUrl = "/api"
      , errors = Nothing
      , loading = 0
      , mdl = Material.model
      , route = Route.Empty
      , tabs = initTabs
      , token = Nothing
      , authMgr = Nothing
      }
    , Layout.sub0 Mdl
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
                |> Meld.send Results (\_ -> model.actions) (\ac -> { model | actions = model.actions + ac })

        ActSeq tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.sequence Results (\_ -> model.actions) (\ac -> { model | actions = model.actions + ac })

        Mdl mm ->
            Material.update Mdl mm model

        Request tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Results (\_ -> model.loading) (\tc -> { model | loading = model.loading + tc })

        Responses taskCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        taskCount
                        (\_ -> model.loading)
                        (\tc -> { model | loading = tc })
                        model
                        meld

                Err meldError ->
                    let
                        _ =
                            Debug.log "Request Error" meldError
                    in
                    ( { model | errors = Just <| Meld.errorMessage meldError }
                    , Cmd.none
                    )

        Results actCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        actCount
                        (\_ -> model.actions)
                        (\ac -> { model | actions = model.actions - ac })
                        model
                        meld

                Err meldError ->
                    let
                        _ =
                            Debug.log "Action Error" meldError
                    in
                    ( { model | errors = Just <| Meld.errorMessage meldError }
                    , Cmd.none
                    )

        SelectTab fragment ->
            Route.tabs model.token model.tabs
                |> List.filter (\t -> fragment == Route.toFragment t.route)
                |> List.head
                |> Maybe.map
                    (\t -> ( { model | route = t.route }, Cmd.none ))
                |> Maybe.withDefault
                    ( { model | route = Route.defaultRoute model.token }
                    , Navigation.modifyUrl (Route.defaultRoute model.token |> Route.toFragment)
                    )

        TextInput updateFn m v ->
            updateFn m v


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Layout.subs Mdl model.mdl
        ]
