module View
    exposing
        ( view
        )

import Dict exposing (Dict)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style, type_, value)
import Material.Color as Color
import Material.Layout as Layout
import Material.Options as Options
import Messages exposing (Msg(..))
import Model exposing (Model)
import Route exposing (Route(..), Tab)
import View.Login as Login
import View.Logout as Logout


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.fixedTabs
        , Layout.selectedTab 0
        ]
        { header = layoutHeader
        , drawer = layoutDrawer model.token model.tabs
        , tabs =
            ( layoutTab model.route model.tabs
            , [ Color.background Color.primary
              , Color.text Color.black
              ]
            )
        , main = routeLayout model
        }


layoutHeader : List (Html Msg)
layoutHeader =
    [ Layout.row
        []
        [ Layout.title [] [ text "Expense Manager" ]
        , Layout.spacer
        ]
    ]


layoutDrawer : Maybe String -> Dict String Tab -> List (Html Msg)
layoutDrawer token tabs =
    [ Layout.title [] [ text "Expense Manager" ]
    , Layout.navigation
        []
        (Route.tabs token tabs
            |> List.map
                (\t ->
                    Layout.link
                        [ Layout.href <| Route.toFragment t.route
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text t.label ]
                )
        )
    ]


layoutTab : Route -> Dict String Tab -> List (Html Msg)
layoutTab route tabs =
    tabs
        |> Dict.get (Route.toFragment route)
        |> Maybe.map (\t -> [ text t.label ])
        |> Maybe.withDefault []


routeLayout : Model -> List (Html Msg)
routeLayout model =
    [ div
        [ style
            [ ( "padding", "10px" )
            ]
        ]
        (case model.route of
            Empty ->
                []

            Login ->
                [ Login.view model
                ]

            Logout ->
                [ Logout.view model
                ]

            Currencies ->
                []

            Accounts ->
                []

            Categories ->
                []

            Transactions ->
                []

            Statistics ->
                []

            Groups ->
                []
        )
    ]
