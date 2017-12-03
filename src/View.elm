module View
    exposing
        ( view
        )

import Api.Auth
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Manager.Auth
import Material.Layout as Layout
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div [] []


viewLogin : Model -> Html Msg
viewLogin model =
    div []
        [ text "Expense Manager"
        , div
            []
            [ text "Username"
            ]
        , div
            []
            [ input
                [ TextInput Manager.Auth.emailInput model
                    |> onInput
                , type_ "text"
                , model.authMgr
                    |> Maybe.map .email
                    |> Maybe.withDefault ""
                    |> value
                ]
                []
            ]
        , div
            []
            [ text "Password"
            ]
        , div
            []
            [ input
                [ TextInput Manager.Auth.passInput model
                    |> onInput
                , type_ "password"
                , model.authMgr
                    |> Maybe.map .pass
                    |> Maybe.withDefault ""
                    |> value
                ]
                []
            ]
        , div
            []
            [ input
                [ onClick <| Act Api.Auth.login
                , type_ "button"
                , value "Login"
                ]
                []
            ]
        ]
