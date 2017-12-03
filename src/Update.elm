module Update
    exposing
        ( delta2url
        , init
        , subscriptions
        , update
        , url2messages
        )

import Material
import Meld exposing (Error(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
import Navigation exposing (Location)
import Route
import RouteUrl exposing (UrlChange)
import Ui


init : ( Model, Cmd Msg )
init =
    ( { actions = 0
      , apiBaseUrl = "/api"
      , loading = 0
      , mdl = Material.model
      , route = Route.Login
      , token = Nothing
      , authMgr = Nothing
      }
    , Cmd.none
    )


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    Debug.log "delta2url" Nothing


url2messages : Location -> List Msg
url2messages location =
    []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Act task ->
            Ui.act model [ task ]

        ActSeq task ->
            Ui.actSequence model [ task ]

        Actions actCount result ->
            case result of
                Ok meld ->
                    Meld.update
                        actCount
                        (\_ -> model.actions)
                        (\ac -> { model | actions = ac })
                        model
                        meld

                Err meldError ->
                    let
                        _ =
                            Debug.log "Action Error" meldError
                    in
                    ( model
                    , Cmd.none
                    )

        Mdl mm ->
            Material.update Mdl mm model

        Requests taskCount result ->
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
                    ( model
                    , Cmd.none
                    )

        TextInput updateFn m v ->
            updateFn m v


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
