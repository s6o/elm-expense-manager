module View.Login
    exposing
        ( view
        )

import Api.Auth
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Auth as Auth exposing (Auth)
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))


view : Material.Model -> Auth -> Html Msg
view mdl auth =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ 0 ]
                mdl
                [ Textfield.label "Enter email"
                , Textfield.autofocus
                , Textfield.floatingLabel
                , Textfield.email
                , css "width" "100%"
                , Auth.email auth
                    |> Textfield.value
                , TextInput (Auth.fieldInput Auth.Email)
                    |> Options.onInput
                , KeyEvent.onEnter <| Act [ Api.Auth.login ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 1 ]
                mdl
                [ Textfield.label "Enter password"
                , Textfield.floatingLabel
                , Textfield.password
                , css "width" "100%"
                , Auth.pass auth
                    |> Textfield.value
                , TextInput (Auth.fieldInput Auth.Pass)
                    |> Options.onInput
                , KeyEvent.onEnter <| Act [ Api.Auth.login ]
                ]
                []
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ 2 ]
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Act [ Api.Auth.login ]
                ]
                [ text "Login" ]
            ]
        ]
