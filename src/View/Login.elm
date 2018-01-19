module View.Login
    exposing
        ( view
        )

import Api.Auth
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Auth as Auth
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "Enter email"
                , Textfield.autofocus
                , Textfield.floatingLabel
                , Textfield.email
                , css "width" "100%"
                , Auth.email model.auth
                    |> Textfield.value
                , TextInput (Auth.fieldInput "email")
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Auth.login ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Enter password"
                , Textfield.floatingLabel
                , Textfield.password
                , css "width" "100%"
                , Auth.pass model.auth
                    |> Textfield.value
                , TextInput (Auth.fieldInput "pass")
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Auth.login ]
                ]
                []
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ 2 ]
                model.mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Request [ Api.Auth.login ]
                ]
                [ text "Login" ]
            ]
        ]
