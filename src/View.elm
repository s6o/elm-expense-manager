module View
    exposing
        ( view
        )

import Api.Auth
import Dict
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Manager.Auth
import Material.Color as Color
import Material.Layout as Layout
import Material.Options as Options
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.selectedTab model.tabIndex
        , Layout.onSelectTab SelectTab
        ]
        { header = layoutHeader
        , drawer = layoutDrawer
        , tabs =
            ( model.tabs
                |> Dict.values
                |> List.map text
            , [ Color.background Color.primary
              , Color.text Color.black
              ]
            )
        , main = []
        }


layoutHeader : List (Html Msg)
layoutHeader =
    [ Layout.row
        []
        [ Layout.title [] [ text "Expense Manager" ]
        , Layout.spacer
        ]
    ]


layoutDrawer : List (Html Msg)
layoutDrawer =
    [ Layout.title [] [ text "Expense Manager" ]
    , Layout.navigation
        []
        [ Layout.link
            [ Layout.href "https://github.com/s6o/elm-expense-manager" ]
            [ text "github" ]
        , Layout.link
            [ Layout.href "#settings"
            , Options.onClick (Layout.toggleDrawer Mdl)
            ]
            [ text "Settings" ]
        ]
    ]


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
