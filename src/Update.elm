module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Api
import Api.Auth
import Meld exposing (Error(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
import Time
import Ui


init : ( Model, Cmd Msg )
init =
    ( { actions = 0
      , apiBaseUrl = "/api"
      , loading = 0
      , systemTick = 0
      , token = Nothing
      , authMgr = Nothing
      }
    , Cmd.none
    )


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

        Login ->
            model.authMgr
                |> Maybe.map
                    (\mgr ->
                        if String.length mgr.email > 0 && String.length mgr.pass > 0 then
                            Api.call model [ Api.Auth.login ]
                        else
                            ( model
                            , Cmd.none
                            )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        SystemTick t ->
            ( { model | systemTick = t }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (256 * Time.millisecond) SystemTick
        ]
